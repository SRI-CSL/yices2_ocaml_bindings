#!/usr/bin/env bash
set -euo pipefail

project_root=""
prefix=""
stamp=""

while [ $# -gt 0 ]; do
  case "$1" in
    --project-root)
      project_root="$2"
      shift 2
      ;;
    --prefix)
      prefix="$2"
      shift 2
      ;;
    --stamp)
      stamp="$2"
      shift 2
      ;;
    *)
      echo "Unknown argument: $1" >&2
      exit 2
      ;;
  esac
done

if [ -z "$project_root" ]; then
  echo "Usage: $0 --project-root <path> --prefix <path> [--stamp <path>]" >&2
  exit 2
fi

project_root="$(cd "$project_root" && pwd)"

os_name="$(uname -s 2>/dev/null || echo unknown)"
case "$os_name" in
  Darwin) platform="macos" ;;
  Linux) platform="linux" ;;
  *) platform="unknown" ;;
esac

if [ -z "$prefix" ]; then
  prefix="$project_root/_build/vendor_install"
fi

if [[ "${prefix:0:1}" != "/" ]]; then
  prefix="$project_root/$prefix"
fi

mkdir -p "$prefix"
prefix="$(cd "$prefix" && pwd)"

check_mcsat() {
  local cc cflags libs tmp_dir c_file exe_file libpaths flag path rc pkgconfig_path opam_prefix opam_cmd opam_root opam_switch pkg_config_cmd

  if [[ -z "${HOME:-}" ]]; then
    HOME="$(eval echo ~)"
  fi

  cc="${CC:-cc}"
  tmp_dir="$(mktemp -d)"
  c_file="$tmp_dir/has_mcsat.c"
  exe_file="$tmp_dir/has_mcsat"

  cat > "$c_file" <<'EOF'
#include <yices.h>
int main(void) {
  return yices_has_mcsat() ? 0 : 1;
}
EOF

  opam_prefix="${OPAM_SWITCH_PREFIX:-}"
  opam_cmd=""
  if command -v opam >/dev/null 2>&1; then
    opam_cmd="$(command -v opam)"
  else
    for candidate in /opt/homebrew/bin/opam /usr/local/bin/opam /opt/local/bin/opam; do
      if [ -x "$candidate" ]; then
        opam_cmd="$candidate"
        break
      fi
    done
  fi

  if [[ -z "$opam_prefix" ]] && [[ -n "$opam_cmd" ]]; then
    opam_prefix="$("$opam_cmd" var prefix 2>/dev/null || true)"
  fi
  if [[ -z "$opam_prefix" ]] && [ -f "$HOME/.opam/config" ]; then
    opam_switch="$(awk -F'\"' '/^switch:/ {print $2; exit}' "$HOME/.opam/config")"
    if [[ -n "$opam_switch" ]]; then
      opam_root="${OPAMROOT:-$HOME/.opam}"
      opam_prefix="$opam_root/$opam_switch"
    fi
  fi

  pkg_config_cmd=""
  if command -v pkg-config >/dev/null 2>&1; then
    pkg_config_cmd="pkg-config"
  elif command -v pkgconf >/dev/null 2>&1; then
    pkg_config_cmd="pkgconf"
  fi

  if [[ -n "$pkg_config_cmd" ]]; then
    pkgconfig_path="${PKG_CONFIG_PATH:-}"
    if [[ -n "$opam_prefix" ]]; then
      pkgconfig_path="${opam_prefix}/lib/pkgconfig${pkgconfig_path:+:${pkgconfig_path}}"
    fi
    if PKG_CONFIG_PATH="$pkgconfig_path" "$pkg_config_cmd" --exists yices; then
      cflags="$(PKG_CONFIG_PATH="$pkgconfig_path" "$pkg_config_cmd" --cflags yices)"
      libs="$(PKG_CONFIG_PATH="$pkgconfig_path" "$pkg_config_cmd" --libs yices)"
    fi
  fi

  if [[ -z "${libs:-}" ]]; then
    if [[ -n "$opam_prefix" ]] && [ -f "$opam_prefix/include/yices.h" ] \
       && compgen -G "$opam_prefix/lib/libyices.*" > /dev/null; then
      cflags="-I$opam_prefix/include"
      libs="-L$opam_prefix/lib -lyices"
    fi
  fi

  if [[ -z "${libs:-}" ]]; then
    rm -rf "$tmp_dir"
    return 1
  fi

  if ! $cc $cflags "$c_file" $libs -o "$exe_file" >/dev/null 2>&1; then
    rm -rf "$tmp_dir"
    return 1
  fi

  libpaths=""
  for flag in $libs; do
    case "$flag" in
      -L*) path="${flag#-L}"; libpaths="${libpaths:+$libpaths:}$path" ;;
    esac
  done

  if [ -n "$libpaths" ]; then
    if [ "$platform" = "macos" ]; then
      LD_LIBRARY_PATH="$libpaths${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
      DYLD_LIBRARY_PATH="$libpaths${DYLD_LIBRARY_PATH:+:$DYLD_LIBRARY_PATH}" \
      "$exe_file" >/dev/null 2>&1
      rc=$?
    else
      LD_LIBRARY_PATH="$libpaths${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
      "$exe_file" >/dev/null 2>&1
      rc=$?
    fi
  else
    "$exe_file" >/dev/null 2>&1
    rc=$?
  fi

  rm -rf "$tmp_dir"
  return $rc
}

if [ "${YICES2_FORCE_LOCAL:-}" != "1" ] && check_mcsat; then
  echo "Using system Yices via pkg-config (MCSAT enabled); skipping vendored build."
  touch "$prefix/.keep"
  if [ -n "$stamp" ]; then
    mkdir -p "$(dirname "$stamp")"
    printf '%s\n' "yices via pkg-config (mcsat enabled)" > "$stamp"
  fi
  exit 0
fi

yices_dir="$project_root/vendor/yices2"
cudd_dir="$project_root/vendor/cudd"
install_lib="$prefix/lib"
build_root="${VENDOR_BUILD_ROOT:-$PWD/_vendor_build}"
mkdir -p "$build_root"
build_root="$(cd "$build_root" && pwd)"

lock_dir="$build_root/.vendor_build_lock"
while ! mkdir "$lock_dir" 2>/dev/null; do
  sleep 1
done
trap 'rmdir "$lock_dir"' EXIT

make_cmd="make"
if command -v gmake >/dev/null 2>&1; then
  make_cmd="gmake"
fi

if [ ! -d "$yices_dir" ] || [ ! -d "$cudd_dir" ]; then
  echo "Missing submodules in vendor/. Run: git submodule update --init --recursive" >&2
  exit 1
fi

if [ -f "$install_lib/libyices.a" ] && [ -f "$install_lib/libcudd.a" ] \
   && [ -f "$prefix/include/yices.h" ] && [ -f "$prefix/include/cudd.h" ]; then
  echo "Found vendored Yices/CUDD in $prefix; skipping vendored build."
  touch "$prefix/.keep"
  if [ -n "$stamp" ]; then
    mkdir -p "$(dirname "$stamp")"
    printf '%s\n' "yices already installed in vendor prefix" > "$stamp"
  fi
  exit 0
fi

echo "Building vendored CUDD/Yices2 into $prefix"

mkdir -p "$prefix"
export CPPFLAGS="-I$prefix/include ${CPPFLAGS:-}"
export LDFLAGS="-L$prefix/lib ${LDFLAGS:-}"

# If libpoly is installed in the current opam switch, add it to the search path
# so Yices' configure can find libpoly.a for MCSAT.
opam_prefix="${OPAM_SWITCH_PREFIX:-}"
if [[ -z "$opam_prefix" ]]; then
  if command -v opam >/dev/null 2>&1; then
    opam_prefix="$(opam var prefix 2>/dev/null || true)"
  elif [ -f "${HOME:-$project_root}/.opam/config" ]; then
    opam_switch="$(awk -F'\"' '/^switch:/ {print $2; exit}' "${HOME:-$project_root}/.opam/config")"
    if [[ -n "$opam_switch" ]]; then
      opam_root="${OPAMROOT:-${HOME:-$project_root}/.opam}"
      opam_prefix="$opam_root/$opam_switch"
    fi
  fi
fi

libpoly_shared_glob=""
case "$platform" in
  macos) libpoly_shared_glob="libpoly*.dylib" ;;
  linux) libpoly_shared_glob="libpoly.so*" ;;
  *) libpoly_shared_glob="libpoly.so* libpoly*.dylib" ;;
esac

libpoly_prefix_candidates=()
if [[ -n "$opam_prefix" ]]; then
  libpoly_prefix_candidates+=("$opam_prefix")
fi
if [[ -n "${LIBPOLY_PREFIX:-}" ]]; then
  libpoly_prefix_candidates+=("${LIBPOLY_PREFIX}")
fi
if [[ -n "${LIBPOLY_VENDOR_PREFIX:-}" ]]; then
  libpoly_prefix_candidates+=("${LIBPOLY_VENDOR_PREFIX}")
fi
libpoly_prefix_candidates+=("$project_root/../libpoly_ocaml_bindings/_build/default/vendor_install")

libpoly_lib_dirs=()
libpoly_prefix=""
for candidate in "${libpoly_prefix_candidates[@]}"; do
  if [[ -z "$candidate" ]] || [ ! -d "$candidate" ]; then
    continue
  fi
  if compgen -G "$candidate/lib/$libpoly_shared_glob" > /dev/null; then
    libpoly_prefix="$candidate"
    libpoly_lib_dirs+=("$candidate/lib")
    break
  fi
  if compgen -G "$candidate/lib/stublibs/$libpoly_shared_glob" > /dev/null; then
    libpoly_prefix="$candidate"
    libpoly_lib_dirs+=("$candidate/lib/stublibs")
    break
  fi
done

if [[ -z "$libpoly_prefix" ]] && [[ -n "$opam_prefix" ]] && [ -f "$opam_prefix/lib/libpoly.a" ]; then
  echo "Found only static libpoly.a in $opam_prefix/lib; refusing to link it into libyices." >&2
  echo "Install a shared libpoly (libpoly.dylib/.so) or set LIBPOLY_PREFIX to a shared libpoly prefix." >&2
  exit 1
fi

if [ ${#libpoly_lib_dirs[@]} -gt 0 ]; then
  export CPPFLAGS="-I$libpoly_prefix/include ${CPPFLAGS:-}"
  for libdir in "${libpoly_lib_dirs[@]}"; do
    export LDFLAGS="-L$libdir ${LDFLAGS:-}"
  done
fi

if [ -x "$cudd_dir/configure" ]; then
  cudd_build="$build_root/cudd"
  mkdir -p "$cudd_build"
  cudd_cflags="${CFLAGS:-} -Wno-unused-but-set-variable -Wno-unused-variable"
  (cd "$cudd_build" && DOXYGEN=true CFLAGS="$cudd_cflags" lt_cv_sys_max_cmd_len=262144 \
     "$cudd_dir/configure" --prefix="$prefix" --enable-static --disable-shared)
else
  echo "CUDD configure script not found at $cudd_dir/configure" >&2
  exit 1
fi

$make_cmd -C "$cudd_build" all-am
$make_cmd -C "$cudd_build" install

  yices_src="$build_root/yices2-src"
  if [ ! -d "$yices_src" ]; then
    cp -R "$yices_dir" "$yices_src"
  fi
  if [ ! -x "$yices_src/configure" ]; then
    if ! command -v autoconf >/dev/null 2>&1; then
      echo "Yices configure script not found and autoconf is missing." >&2
      exit 1
    fi
    (cd "$yices_src" && autoconf -W none)
  fi
  (cd "$yices_src" && ./configure --prefix="$prefix" --enable-mcsat ${YICES2_CONFIGURE_FLAGS:-})

yices_mode="release"
tmp_yices_log="$(mktemp)"
if ! $make_cmd -C "$yices_src" lib 2> "$tmp_yices_log"; then
  cat "$tmp_yices_log" >&2
  rm -f "$tmp_yices_log"
  exit 1
fi
grep -v -E "Makefile:[0-9]+: .*\\.d: No such file or directory" "$tmp_yices_log" >&2 || true
rm -f "$tmp_yices_log"

yices_build_dir="$(ls -d "$yices_src"/build/*-"$yices_mode" 2>/dev/null | head -n 1)"
if [ -z "$yices_build_dir" ]; then
  echo "Yices build directory not found under $yices_src/build" >&2
  exit 1
fi

yices_libdir="$yices_build_dir/lib"
if [ ! -d "$yices_libdir" ]; then
  echo "Yices library directory not found at $yices_libdir" >&2
  exit 1
fi

mkdir -p "$prefix/include" "$prefix/lib"
cp "$yices_src/src/include/"*.h "$prefix/include/"

for lib in "$yices_libdir"/libyices.*; do
  case "$lib" in
    *.la) continue ;;
  esac
  cp "$lib" "$prefix/lib/"
done

pkgconfig_dir="$prefix/lib/pkgconfig"
mkdir -p "$pkgconfig_dir"
yices_header="$yices_src/src/include/yices.h"
yices_major="$(awk '/__YICES_VERSION[[:space:]]+/{print $3}' "$yices_header" | head -n 1)"
yices_minor="$(awk '/__YICES_VERSION_MAJOR[[:space:]]+/{print $3}' "$yices_header" | head -n 1)"
yices_patch="$(awk '/__YICES_VERSION_PATCHLEVEL[[:space:]]+/{print $3}' "$yices_header" | head -n 1)"
yices_version="${yices_major}.${yices_minor}.${yices_patch}"
cat > "$pkgconfig_dir/yices.pc" <<EOF
prefix=$prefix
exec_prefix=\${prefix}
libdir=\${prefix}/lib
includedir=\${prefix}/include

Name: yices
Description: Yices SMT solver library
Version: $yices_version
Libs: -L\${libdir} -lyices -lcudd -lpoly -lgmp -lm
Cflags: -I\${includedir}
EOF

if [ -f "$prefix/lib/libyices.2.dylib" ] && [ ! -f "$prefix/lib/libyices.dylib" ]; then
  ln -sf "libyices.2.dylib" "$prefix/lib/libyices.dylib"
elif [ -f "$prefix/lib/libyices.dylib" ] && [ ! -f "$prefix/lib/libyices.2.dylib" ]; then
  ln -sf "libyices.dylib" "$prefix/lib/libyices.2.dylib"
fi

if [ -n "$stamp" ]; then
  mkdir -p "$(dirname "$stamp")"
  printf '%s\n' "vendored yices installed" > "$stamp"
fi
