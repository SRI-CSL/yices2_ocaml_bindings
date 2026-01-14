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

if [ ! -d "$from_lib" ] || [ ! -d "$from_inc" ]; then
  echo "No vendored install found at $from_prefix; skipping install."
  exit 0
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

os_name="$(uname -s 2>/dev/null || echo unknown)"
case "$os_name" in
  Darwin)
    if [ -f "$prefix/lib/libyices.2.dylib" ] && [ ! -f "$prefix/lib/libyices.dylib" ]; then
      ln -sf "libyices.2.dylib" "$prefix/lib/libyices.dylib"
    fi
    ;;
  Linux)
    if [ -f "$prefix/lib/libyices.so.2" ] && [ ! -f "$prefix/lib/libyices.so" ]; then
      ln -sf "libyices.so.2" "$prefix/lib/libyices.so"
    fi
    ;;
esac

echo "Installed vendored Yices/CUDD from $from_prefix to $prefix"
