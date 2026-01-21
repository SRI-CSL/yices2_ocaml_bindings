#!/usr/bin/env bash
set -euo pipefail

from_prefix=""
prefix=""

while [ $# -gt 0 ]; do
  case "$1" in
    --from-prefix)
      from_prefix="$2"
      shift 2
      ;;
    --prefix)
      prefix="$2"
      shift 2
      ;;
    *)
      echo "Unknown argument: $1" >&2
      exit 2
      ;;
  esac
done

if [ -z "$from_prefix" ]; then
  echo "Usage: $0 --from-prefix <path> --prefix <path>" >&2
  exit 2
fi

if [ -z "$prefix" ]; then
  if [ -n "${OPAM_SWITCH_PREFIX:-}" ]; then
    prefix="$OPAM_SWITCH_PREFIX"
  elif command -v opam >/dev/null 2>&1; then
    prefix="$(opam var prefix 2>/dev/null || true)"
  fi
fi

if [ -z "$prefix" ]; then
  echo "Missing install prefix (opam switch not detected)." >&2
  exit 1
fi

from_lib="$from_prefix/lib"
from_inc="$from_prefix/include"
skip_copy=0

if [ ! -d "$from_lib" ] || [ ! -d "$from_inc" ]; then
  echo "No vendored install found at $from_prefix; skipping copy."
  skip_copy=1
fi

mkdir -p "$prefix/lib" "$prefix/include" "$prefix/lib/pkgconfig"

copy_lib() {
  local pattern="$1"
  local found=0
  for lib in $pattern; do
    if [ -f "$lib" ]; then
      rm -f "$prefix/lib/$(basename "$lib")"
      cp "$lib" "$prefix/lib/"
      chmod u+w "$prefix/lib/$(basename "$lib")"
      found=1
    fi
  done
  return $found
}

if [ "$skip_copy" -eq 0 ]; then
  copy_lib "$from_lib/libyices.*" || true
  copy_lib "$from_lib/libcudd.*" || true

  for hdr in "$from_inc"/yices*.h "$from_inc"/cudd.h; do
    if [ -f "$hdr" ]; then
      rm -f "$prefix/include/$(basename "$hdr")"
      cp "$hdr" "$prefix/include/"
      chmod u+w "$prefix/include/$(basename "$hdr")"
    fi
  done

  if [ -f "$from_lib/pkgconfig/yices.pc" ]; then
    tmp_pc="$(mktemp)"
    sed -e "s|^prefix=.*|prefix=$prefix|" \
        -e "s|^exec_prefix=.*|exec_prefix=\\${prefix}|" \
        -e "s|^libdir=.*|libdir=\\${prefix}/lib|" \
        -e "s|^includedir=.*|includedir=\\${prefix}/include|" \
        "$from_lib/pkgconfig/yices.pc" > "$tmp_pc"
    rm -f "$prefix/lib/pkgconfig/yices.pc"
    cp "$tmp_pc" "$prefix/lib/pkgconfig/yices.pc"
    chmod u+w "$prefix/lib/pkgconfig/yices.pc"
    rm -f "$tmp_pc"
  fi
fi

os_name="$(uname -s 2>/dev/null || echo unknown)"
case "$os_name" in
  Darwin)
    if [ -f "$prefix/lib/libyices.2.dylib" ]; then
      # Keep the unversioned name pointing at the versioned dylib.
      ln -sf "libyices.2.dylib" "$prefix/lib/libyices.dylib"
    fi
    # macOS dylibs carry an absolute install name from the vendored build.
    # Fix it to the opam prefix and update any OCaml stubs that reference it.
    if command -v otool >/dev/null 2>&1 && command -v install_name_tool >/dev/null 2>&1; then
      fix_install_name() {
        local lib_path="$1"
        local old_install_name new_install_name ocaml_libdir dep_basename

        [ -f "$lib_path" ] || return 0
        old_install_name="$(otool -D "$lib_path" 2>/dev/null | awk 'NR==2 {print $1}' || true)"
        new_install_name="$lib_path"
        dep_basename="$(basename "$lib_path")"
        if [ -n "$old_install_name" ] && [ "$old_install_name" != "$new_install_name" ]; then
          install_name_tool -id "$new_install_name" "$lib_path"
        fi

        patch_dep() {
          local target="$1"
          local dep

          [ -f "$target" ] || return 0
          while IFS= read -r dep; do
            if [ "$dep" = "$new_install_name" ]; then
              continue
            fi
            case "$dep" in
              "$old_install_name"|*/_build/*/vendor_install/*"$dep_basename")
                chmod u+w "$target" 2>/dev/null || true
                install_name_tool -change "$dep" "$new_install_name" "$target"
                ;;
            esac
          done < <(otool -L "$target" 2>/dev/null | awk 'NR>1 {print $1}')
        }

        ocaml_libdir="$(ocamlc -where 2>/dev/null || true)"
        fixup_dirs=("$prefix/lib/stublibs" "$prefix/lib/yices2")
        if [ -n "$ocaml_libdir" ]; then
          fixup_dirs+=("$ocaml_libdir/stublibs" "$ocaml_libdir/yices2")
        fi
        for dir in "${fixup_dirs[@]}"; do
          [ -d "$dir" ] || continue
          while IFS= read -r -d '' candidate; do
            patch_dep "$candidate"
          done < <(find "$dir" -type f \( -name "*.so" -o -name "*.dylib" \) -print0)
        done
      }

      fix_install_name "$prefix/lib/libyices.2.dylib"
      fix_install_name "$prefix/lib/libyices.dylib"
    fi
    ;;
  Linux)
    if compgen -G "$prefix/lib/libyices.so."* > /dev/null && [ ! -f "$prefix/lib/libyices.so" ]; then
      so_target="$(ls -1 "$prefix/lib/libyices.so."* | head -n 1)"
      ln -sf "$(basename "$so_target")" "$prefix/lib/libyices.so"
    fi
    ;;
esac

if [ "$skip_copy" -eq 0 ]; then
  echo "Installed vendored Yices/CUDD from $from_prefix to $prefix"
else
  echo "Applied Yices/CUDD install-name fixups under $prefix"
fi
