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

if [ -z "$project_root" ] || [ -z "$stamp" ]; then
  echo "Usage: $0 --project-root <path> --prefix <path> --stamp <path>" >&2
  exit 2
fi

if [ -z "$prefix" ]; then
  if [ -n "${OPAM_SWITCH_PREFIX:-}" ]; then
    prefix="$OPAM_SWITCH_PREFIX"
  elif command -v opam >/dev/null 2>&1; then
    prefix="$(opam var prefix 2>/dev/null || true)"
  fi
  if [ -z "$prefix" ]; then
    prefix="$project_root/vendor/_install"
  fi
fi

check_mcsat() {
  local cc cflags libs tmp_dir c_file exe_file libpaths flag path rc

  if ! command -v pkg-config >/dev/null 2>&1; then
    return 1
  fi
  if ! pkg-config --exists yices; then
    return 1
  fi

  cc="${CC:-cc}"
  cflags="$(pkg-config --cflags yices)"
  libs="$(pkg-config --libs yices)"
  tmp_dir="$(mktemp -d)"
  c_file="$tmp_dir/has_mcsat.c"
  exe_file="$tmp_dir/has_mcsat"

  cat > "$c_file" <<'EOF'
#include <yices.h>
int main(void) {
  return yices_has_mcsat() ? 0 : 1;
}
EOF

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
    LD_LIBRARY_PATH="$libpaths${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
    DYLD_LIBRARY_PATH="$libpaths${DYLD_LIBRARY_PATH:+:$DYLD_LIBRARY_PATH}" \
    "$exe_file" >/dev/null 2>&1
    rc=$?
  else
    "$exe_file" >/dev/null 2>&1
    rc=$?
  fi

  rm -rf "$tmp_dir"
  return $rc
}

if [ "${YICES2_FORCE_LOCAL:-}" != "1" ] && check_mcsat; then
  echo "Using system Yices via pkg-config (MCSAT enabled); skipping vendored build."
  mkdir -p "$(dirname "$stamp")"
  printf '%s\n' "yices via pkg-config (mcsat enabled)" > "$stamp"
  exit 0
fi

yices_dir="$project_root/vendor/yices2"
cudd_dir="$project_root/vendor/cudd"
install_lib="$prefix/lib"

if [ ! -d "$yices_dir" ] || [ ! -d "$cudd_dir" ]; then
  echo "Missing submodules in vendor/. Run: git submodule update --init --recursive" >&2
  exit 1
fi

if [ -f "$install_lib/libyices.a" ] || [ -f "$install_lib/libyices.so" ] || [ -f "$install_lib/libyices.dylib" ]; then
  echo "Found Yices in $prefix; skipping vendored build."
  mkdir -p "$(dirname "$stamp")"
  printf '%s\n' "yices already installed in vendor prefix" > "$stamp"
  exit 0
fi

echo "Building vendored CUDD/Yices2 into $prefix"

mkdir -p "$prefix"
export CPPFLAGS="-I$prefix/include ${CPPFLAGS:-}"
export LDFLAGS="-L$prefix/lib ${LDFLAGS:-}"

if [ -x "$cudd_dir/configure" ]; then
  (cd "$cudd_dir" && ./configure --prefix="$prefix" --enable-static --disable-shared)
else
  echo "CUDD configure script not found at $cudd_dir/configure" >&2
  exit 1
fi

make -C "$cudd_dir"
make -C "$cudd_dir" install

if [ -x "$yices_dir/configure" ]; then
  (cd "$yices_dir" && ./configure --prefix="$prefix" --enable-mcsat --enable-static ${YICES2_CONFIGURE_FLAGS:-})
else
  echo "Yices configure script not found at $yices_dir/configure" >&2
  exit 1
fi

make -C "$yices_dir"
make -C "$yices_dir" install

mkdir -p "$(dirname "$stamp")"
printf '%s\n' "vendored yices installed" > "$stamp"
