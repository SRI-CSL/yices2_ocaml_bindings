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

enable_mcsat="${YICES2_ENABLE_MCSAT:-1}"
if [ "$enable_mcsat" = "0" ]; then
  enable_mcsat=0
else
  enable_mcsat=1
fi

smt2_static="${YICES2_SMT2_STATIC:-0}"
case "$smt2_static" in
  1|yes|true|TRUE) smt2_static=1 ;;
  *) smt2_static=0 ;;
esac

ensure_linux_yices_links() {
  local libdir="$1"
  local candidate target soname inferred

  target=""
  for candidate in "$libdir"/libyices.so.*; do
    [ -e "$candidate" ] || continue
    [ -f "$candidate" ] || continue
    target="$candidate"
    break
  done
  [ -n "$target" ] || return 0

  soname=""
  if command -v readelf >/dev/null 2>&1; then
    soname="$(readelf -d "$target" 2>/dev/null | sed -n 's/.*Library soname: \[\(.*\)\].*/\1/p' | head -n 1 || true)"
  fi
  if [ -z "$soname" ]; then
    inferred="$(basename "$target" | sed -E 's/^(libyices\.so\.[0-9]+\.[0-9]+)(\..*)?$/\1/')"
    if [ "$inferred" != "$(basename "$target")" ]; then
      soname="$inferred"
    fi
  fi

  if [ -n "$soname" ] && [ ! -e "$libdir/$soname" ]; then
    ln -sf "$(basename "$target")" "$libdir/$soname"
  fi
  if [ ! -e "$libdir/libyices.so" ]; then
    ln -sf "$(basename "$target")" "$libdir/libyices.so"
  fi
}

check_yices() {
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
#if __YICES_VERSION < 2 || (__YICES_VERSION == 2 && __YICES_VERSION_MAJOR < 7)
#error "Yices 2.7 or newer required"
#endif
int main(void) {
#if YICES2_REQUIRE_MCSAT
  return yices_has_mcsat() ? 0 : 1;
#else
  return 0;
#endif
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
    local candidate_prefix
    for candidate_prefix in /opt/homebrew /usr/local /opt/local; do
      if [ -f "$candidate_prefix/include/yices.h" ] && compgen -G "$candidate_prefix/lib/libyices.*" > /dev/null; then
        cflags="-I$candidate_prefix/include"
        libs="-L$candidate_prefix/lib -lyices"
        break
      fi
    done
  fi

  if [[ -z "${libs:-}" ]]; then
    rm -rf "$tmp_dir"
    return 1
  fi

  if ! $cc -DYICES2_REQUIRE_MCSAT="$enable_mcsat" $cflags "$c_file" $libs -o "$exe_file" >/dev/null 2>&1; then
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

fix_system_yices_libpoly() {
  # macOS: ensure opam's libyices doesn't point at a build-tree libpoly path.
  [ "$platform" = "macos" ] || return 0
  command -v otool >/dev/null 2>&1 || return 0
  command -v install_name_tool >/dev/null 2>&1 || return 0

  local opam_prefix opam_cmd opam_root opam_switch libyices_path libpoly_path old_dep
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

  if [[ -z "$opam_prefix" ]]; then
    return 0
  fi

  libyices_path="$opam_prefix/lib/libyices.2.dylib"
  libpoly_path="$opam_prefix/lib/libpoly.0.dylib"
  [ -f "$libyices_path" ] || return 0
  [ -f "$libpoly_path" ] || return 0

  old_dep="$(otool -L "$libyices_path" | awk 'NR>1 && /libpoly/ {print $1; exit}')"
  [ -n "$old_dep" ] || return 0
  case "$old_dep" in
    "$opam_prefix"/*) return 0 ;;
    */_build/*/vendor_install/*) ;;
    *) return 0 ;;
  esac

  chmod u+w "$libyices_path" 2>/dev/null || true
  install_name_tool -change "$old_dep" "$libpoly_path" "$libyices_path"
}

if [ "${YICES2_FORCE_LOCAL:-}" != "1" ] && check_yices; then
  fix_system_yices_libpoly
  if [ "$enable_mcsat" -eq 1 ]; then
    echo "Using system Yices (2.7+, MCSAT enabled); skipping vendored build."
  else
    echo "Using system Yices (2.7+); skipping vendored build."
  fi
  touch "$prefix/.keep"
  if [ -n "$stamp" ]; then
    mkdir -p "$(dirname "$stamp")"
    if [ "$enable_mcsat" -eq 1 ]; then
      printf '%s\n' "yices system (2.7+, mcsat enabled)" > "$stamp"
    else
      printf '%s\n' "yices system (2.7+)" > "$stamp"
    fi
  fi
  exit 0
fi

yices_dir="$project_root/vendor/yices2"
cudd_dir="$project_root/vendor/cudd"
delegates_dir="$project_root/vendor/delegates"
install_lib="$prefix/lib"
build_root="${VENDOR_BUILD_ROOT:-$PWD/_vendor_build}"
mkdir -p "$build_root"
build_root="$(cd "$build_root" && pwd)"

enable_delegates="${YICES2_ENABLE_DELEGATES:-1}"
if [ "$enable_delegates" = "0" ]; then
  enable_delegates=0
else
  enable_delegates=1
fi
without_delegates="${YICES2_WITHOUT_DELEGATES:-${YICES2_WITHOUT_DELEGATE:-}}"
without_delegates="${without_delegates//,/ }"

lock_dir="$build_root/.vendor_build_lock"
while ! mkdir "$lock_dir" 2>/dev/null; do
  sleep 1
done
trap 'rmdir "$lock_dir"' EXIT

make_cmd="make"
if command -v gmake >/dev/null 2>&1; then
  make_cmd="gmake"
fi

detect_build_jobs() {
  local jobs

  jobs="${YICES2_BUILD_JOBS:-${OPAMJOBS:-}}"
  if [[ -n "$jobs" ]]; then
    echo "$jobs"
    return 0
  fi

  case "$platform" in
    macos)
      if command -v sysctl >/dev/null 2>&1; then
        sysctl -n hw.ncpu 2>/dev/null && return 0
      fi
      ;;
    linux)
      if command -v getconf >/dev/null 2>&1; then
        getconf _NPROCESSORS_ONLN 2>/dev/null && return 0
      fi
      ;;
  esac

  echo 2
}

build_jobs="$(detect_build_jobs)"
make_parallel_args=(-j "$build_jobs")

refresh_cudd_autotools_timestamps() {
  local src="$1"
  local file
  local generated=(
    "$src/aclocal.m4"
    "$src/configure"
    "$src/config.h.in"
    "$src/Makefile.in"
  )
  local existing=()

  for file in "${generated[@]}"; do
    if [ -e "$file" ]; then
      existing+=("$file")
    fi
  done

  if [ ${#existing[@]} -gt 0 ]; then
    touch "${existing[@]}"
  fi
}

has_any_file() {
  local pattern="$1"
  compgen -G "$pattern" > /dev/null
}

validate_without_delegates() {
  local delegate

  for delegate in $without_delegates; do
    case "$delegate" in
      cadical|cryptominisat|kissat) ;;
      *)
        echo "Unknown delegate in YICES2_WITHOUT_DELEGATES: $delegate" >&2
        echo "Supported delegates are: cadical cryptominisat kissat" >&2
        exit 2
        ;;
    esac
  done
}

delegate_enabled() {
  local name="$1"
  local disabled

  [ "$enable_delegates" -eq 1 ] || return 1
  for disabled in $without_delegates; do
    if [ "$disabled" = "$name" ]; then
      return 1
    fi
  done
  return 0
}

any_delegate_enabled() {
  delegate_enabled cadical || delegate_enabled cryptominisat || delegate_enabled kissat
}

cudd_installed() {
  [ "$enable_mcsat" -eq 1 ] || return 0

  [ -f "$install_lib/libcudd.a" ] || return 1
  [ -f "$prefix/include/cudd.h" ] || return 1
  return 0
}

delegates_installed() {
  [ "$enable_delegates" -eq 1 ] || return 0

  if delegate_enabled cadical; then
    [ -f "$prefix/include/ccadical.h" ] || return 1
    has_any_file "$install_lib/libcadical.*" || return 1
  fi
  if delegate_enabled cryptominisat; then
    [ -f "$prefix/include/cryptominisat5/cmsat_c.h" ] || return 1
    has_any_file "$install_lib/libcryptominisat5.*" || return 1
    if [ "$smt2_static" -eq 1 ]; then
      [ -f "$install_lib/libcryptominisat5.a" ] || return 1
    fi
  fi
  if delegate_enabled kissat; then
    [ -f "$prefix/include/kissat.h" ] || return 1
    has_any_file "$install_lib/libkissat.*" || return 1
  fi
  return 0
}

vendored_yices_has_delegates() {
  local cc tmp_dir c_file exe_file rc libpaths opam_prefix_probe

  cc="${CC:-cc}"
  tmp_dir="$(mktemp -d)"
  c_file="$tmp_dir/has_delegates.c"
  exe_file="$tmp_dir/has_delegates"

  cat > "$c_file" <<'EOF'
#include <yices.h>
int main(void) {
  if (YICES2_EXPECT_CADICAL != yices_has_delegate("cadical")) return 1;
  if (YICES2_EXPECT_CRYPTOMINISAT != yices_has_delegate("cryptominisat")) return 1;
  if (YICES2_EXPECT_KISSAT != yices_has_delegate("kissat")) return 1;
  return 0;
}
EOF

  local expect_cadical expect_cryptominisat expect_kissat
  if delegate_enabled cadical; then expect_cadical=1; else expect_cadical=0; fi
  if delegate_enabled cryptominisat; then expect_cryptominisat=1; else expect_cryptominisat=0; fi
  if delegate_enabled kissat; then expect_kissat=1; else expect_kissat=0; fi

  if ! $cc \
      -DYICES2_EXPECT_CADICAL="$expect_cadical" \
      -DYICES2_EXPECT_CRYPTOMINISAT="$expect_cryptominisat" \
      -DYICES2_EXPECT_KISSAT="$expect_kissat" \
      -I"$prefix/include" "$c_file" -L"$prefix/lib" -Wl,-rpath,"$prefix/lib" -lyices -o "$exe_file" >/dev/null 2>&1; then
    rm -rf "$tmp_dir"
    return 1
  fi

  opam_prefix_probe="${OPAM_SWITCH_PREFIX:-}"
  if [[ -z "$opam_prefix_probe" ]] && command -v opam >/dev/null 2>&1; then
    opam_prefix_probe="$(opam var prefix 2>/dev/null || true)"
  fi

  libpaths="$prefix/lib"
  if [[ -n "$opam_prefix_probe" ]]; then
    libpaths="$libpaths:$opam_prefix_probe/lib"
  fi

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

  rm -rf "$tmp_dir"
  return $rc
}

vendored_yices_has_mcsat_setting() {
  local cc tmp_dir c_file exe_file rc libpaths opam_prefix_probe

  cc="${CC:-cc}"
  tmp_dir="$(mktemp -d)"
  c_file="$tmp_dir/has_mcsat_setting.c"
  exe_file="$tmp_dir/has_mcsat_setting"

  cat > "$c_file" <<'EOF'
#include <yices.h>
int main(void) {
  return YICES2_EXPECT_MCSAT == yices_has_mcsat() ? 0 : 1;
}
EOF

  if ! $cc -DYICES2_EXPECT_MCSAT="$enable_mcsat" \
      -I"$prefix/include" "$c_file" -L"$prefix/lib" -Wl,-rpath,"$prefix/lib" -lyices -o "$exe_file" >/dev/null 2>&1; then
    rm -rf "$tmp_dir"
    return 1
  fi

  opam_prefix_probe="${OPAM_SWITCH_PREFIX:-}"
  if [[ -z "$opam_prefix_probe" ]] && command -v opam >/dev/null 2>&1; then
    opam_prefix_probe="$(opam var prefix 2>/dev/null || true)"
  fi

  libpaths="$prefix/lib"
  if [[ -n "$opam_prefix_probe" ]]; then
    libpaths="$libpaths:$opam_prefix_probe/lib"
  fi

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

  rm -rf "$tmp_dir"
  return $rc
}

delegate_link_libs() {
  local cxx_runtime

  if [ -n "${YICES2_DELEGATE_LIBS:-}" ]; then
    printf '%s\n' "$YICES2_DELEGATE_LIBS"
    return 0
  fi

  case "$platform" in
    macos) cxx_runtime="-lc++" ;;
    *) cxx_runtime="-lstdc++" ;;
  esac

  local libs=()
  delegate_enabled cryptominisat && libs+=("-lcryptominisat5")
  delegate_enabled cadical && libs+=("-lcadical")
  delegate_enabled kissat && libs+=("-lkissat")
  if [ ${#libs[@]} -gt 0 ]; then
    libs+=("$cxx_runtime" "-lm")
  fi
  if [ "$smt2_static" -eq 1 ] && delegate_enabled cryptominisat; then
    libs+=("-lz" "-pthread")
  fi

  printf '%s\n' "${libs[*]}"
}

remove_disabled_vendor_artifacts() {
  if [ "$enable_mcsat" -eq 0 ]; then
    rm -f "$prefix/lib"/libcudd.* "$prefix/include/cudd.h"
  fi
  if ! delegate_enabled cadical; then
    rm -f "$prefix/lib"/libcadical.* "$prefix/include/ccadical.h"
  fi
  if ! delegate_enabled cryptominisat; then
    rm -f "$prefix/lib"/libcryptominisat5.*
    rm -rf "$prefix/include/cryptominisat5"
  fi
  if ! delegate_enabled kissat; then
    rm -f "$prefix/lib"/libkissat.* "$prefix/include/kissat.h"
  fi
}

prepare_delegate_source() {
  local src="$1"
  local dst="$2"

  rm -rf "$dst"
  mkdir -p "$(dirname "$dst")"
  cp -R "$src" "$dst"
  rm -rf "$dst/.git"
}

fix_delegate_install_names() {
  [ "$platform" = "macos" ] || return 0
  command -v install_name_tool >/dev/null 2>&1 || return 0

  local lib
  for lib in "$prefix/lib/libcadical.so" "$prefix/lib/libkissat.so" "$prefix/lib"/libcryptominisat5*.dylib; do
    [ -f "$lib" ] || continue
    install_name_tool -id "@rpath/$(basename "$lib")" "$lib"
  done
}

build_delegates() {
  any_delegate_enabled || return 0

  local cadical_src cryptominisat_src kissat_src
  local cadical_build_src cryptominisat_build_src kissat_build_src
  local cryptominisat_build cryptominisat_static_build

  cadical_src="$delegates_dir/src/cadical"
  cryptominisat_src="$delegates_dir/src/cryptominisat"
  kissat_src="$delegates_dir/src/kissat"

  if delegate_enabled cadical && [ ! -d "$cadical_src" ]; then
    echo "Missing CaDiCaL submodule in vendor/delegates. Run: git submodule update --init --recursive" >&2
    exit 1
  fi
  if delegate_enabled cryptominisat && [ ! -d "$cryptominisat_src" ]; then
    echo "Missing CryptoMiniSAT submodule in vendor/delegates. Run: git submodule update --init --recursive" >&2
    exit 1
  fi
  if delegate_enabled kissat && [ ! -d "$kissat_src" ]; then
    echo "Missing Kissat submodule in vendor/delegates. Run: git submodule update --init --recursive" >&2
    exit 1
  fi
  if delegate_enabled cryptominisat && ! command -v cmake >/dev/null 2>&1; then
    echo "Building CryptoMiniSAT requires cmake." >&2
    exit 1
  fi

  echo "Building vendored delegate SAT solvers into $prefix"
  mkdir -p "$prefix/include" "$prefix/lib" "$prefix/bin"

  if delegate_enabled cadical; then
    cadical_build_src="$build_root/delegates/cadical"
    prepare_delegate_source "$cadical_src" "$cadical_build_src"
    (cd "$cadical_build_src" && CXXFLAGS="${CXXFLAGS:-} -fPIC" CFLAGS="${CFLAGS:-} -fPIC" ./configure)
    $make_cmd -C "$cadical_build_src/build" "${make_parallel_args[@]}"
    install -m644 "$cadical_build_src/build/libcadical.a" "$prefix/lib/"
    install -m644 "$cadical_build_src/src/ccadical.h" "$prefix/include/"
    $make_cmd -C "$cadical_build_src/build" libcadical.so
    install -m755 "$cadical_build_src/build/libcadical.so" "$prefix/lib/"
  fi

  if delegate_enabled cryptominisat; then
    cryptominisat_build_src="$build_root/delegates/cryptominisat"
    cryptominisat_build="$build_root/delegates/cryptominisat-build"
    prepare_delegate_source "$cryptominisat_src" "$cryptominisat_build_src"
    rm -rf "$cryptominisat_build"
    cmake -S "$cryptominisat_build_src" -B "$cryptominisat_build" \
      -DCMAKE_BUILD_TYPE=Release \
      -DCMAKE_INSTALL_PREFIX="$prefix" \
      -DCMAKE_POSITION_INDEPENDENT_CODE=ON \
      -DBUILD_SHARED_LIBS=ON \
      -DENABLE_TESTING=OFF \
      -DENABLE_PYTHON_INTERFACE=OFF \
      -DONLY_SIMPLE=ON \
      -DCMAKE_POLICY_VERSION_MINIMUM=3.5
    cmake --build "$cryptominisat_build" --parallel "$build_jobs"
    cmake --install "$cryptominisat_build"
    if [ "$smt2_static" -eq 1 ]; then
      cryptominisat_static_build="$build_root/delegates/cryptominisat-static-build"
      rm -rf "$cryptominisat_static_build"
      cmake -S "$cryptominisat_build_src" -B "$cryptominisat_static_build" \
        -DCMAKE_BUILD_TYPE=Release \
        -DCMAKE_INSTALL_PREFIX="$prefix" \
        -DCMAKE_POSITION_INDEPENDENT_CODE=ON \
        -DBUILD_SHARED_LIBS=OFF \
        -DENABLE_TESTING=OFF \
        -DENABLE_PYTHON_INTERFACE=OFF \
        -DONLY_SIMPLE=ON \
        -DCMAKE_POLICY_VERSION_MINIMUM=3.5
      cmake --build "$cryptominisat_static_build" --parallel "$build_jobs"
      cmake --install "$cryptominisat_static_build"
    fi
  fi

  if delegate_enabled kissat; then
    kissat_build_src="$build_root/delegates/kissat"
    prepare_delegate_source "$kissat_src" "$kissat_build_src"
    (cd "$kissat_build_src" && ./configure -fPIC)
    $make_cmd -C "$kissat_build_src/build" "${make_parallel_args[@]}"
    install -m644 "$kissat_build_src/build/libkissat.a" "$prefix/lib/"
    install -m644 "$kissat_build_src/src/kissat.h" "$prefix/include/"
    $make_cmd -C "$kissat_build_src/build" libkissat.so
    install -m755 "$kissat_build_src/build/libkissat.so" "$prefix/lib/"
  fi

  fix_delegate_install_names
}

validate_without_delegates
remove_disabled_vendor_artifacts

if [ ! -d "$yices_dir" ] || { [ "$enable_mcsat" -eq 1 ] && [ ! -d "$cudd_dir" ]; }; then
  echo "Missing submodules in vendor/. Run: git submodule update --init --recursive" >&2
  exit 1
fi

if [ -f "$install_lib/libyices.a" ] \
   && [ -f "$prefix/include/yices.h" ] \
   && cudd_installed && delegates_installed \
   && vendored_yices_has_mcsat_setting && vendored_yices_has_delegates; then
  echo "Found vendored Yices/CUDD/delegate configuration in $prefix; skipping vendored build."
  touch "$prefix/.keep"
  if [ -n "$stamp" ]; then
    mkdir -p "$(dirname "$stamp")"
    printf '%s\n' "yices already installed in vendor prefix" > "$stamp"
  fi
  exit 0
fi

echo "Building vendored Yices2 configuration into $prefix"

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
# Keep the search local to opam or explicit prefixes; no implicit sibling repos.
if [[ -n "${LIBPOLY_VENDOR_PREFIX:-}" ]]; then
  libpoly_prefix_candidates+=("${LIBPOLY_VENDOR_PREFIX}")
fi

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

if [ "$enable_mcsat" -eq 1 ]; then
  if [ -x "$cudd_dir/configure" ]; then
    cudd_build="$build_root/cudd"
    mkdir -p "$cudd_build"
    cudd_pic_cflags=""
    case "$platform" in
      linux) cudd_pic_cflags="-fPIC" ;;
    esac
    cudd_cflags="${CFLAGS:-} $cudd_pic_cflags -Wno-unused-but-set-variable -Wno-unused-variable"
    cudd_maintainer_vars=(ACLOCAL=: AUTOCONF=: AUTOMAKE=: AUTOHEADER=:)
    # CUDD ships generated Autotools files.  Keep Automake's maintainer
    # rebuild rules dormant even when a checkout/copy gives inputs newer mtimes.
    refresh_cudd_autotools_timestamps "$cudd_dir"
    (cd "$cudd_build" && env DOXYGEN=true CFLAGS="$cudd_cflags" lt_cv_sys_max_cmd_len=262144 \
       "${cudd_maintainer_vars[@]}" \
       "$cudd_dir/configure" --prefix="$prefix" --enable-static --disable-shared)
    # CFLAGS changes are not dependency-tracked, so rebuild any stale non-PIC
    # objects left by a previous failed vendored build.
    $make_cmd -C "$cudd_build" clean "${cudd_maintainer_vars[@]}"
  else
    echo "CUDD configure script not found at $cudd_dir/configure" >&2
    exit 1
  fi

  $make_cmd -C "$cudd_build" "${make_parallel_args[@]}" all-am "${cudd_maintainer_vars[@]}"
  $make_cmd -C "$cudd_build" install "${cudd_maintainer_vars[@]}"
fi

build_delegates

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
delegate_cppflags=""
delegate_ldflags=""
delegate_libs=""
cudd_libs=""
if any_delegate_enabled; then
  delegate_enabled cadical && delegate_cppflags="$delegate_cppflags -DHAVE_CADICAL"
  delegate_enabled cryptominisat && delegate_cppflags="$delegate_cppflags -DHAVE_CRYPTOMINISAT"
  delegate_enabled kissat && delegate_cppflags="$delegate_cppflags -DHAVE_KISSAT"
  delegate_ldflags="-Wl,-rpath,$prefix/lib"
  delegate_libs="$(delegate_link_libs)"
fi
configure_args=(--prefix="$prefix")
if [ "$enable_mcsat" -eq 1 ]; then
  configure_args+=(--enable-mcsat)
  cudd_libs="-lcudd"
fi
(cd "$yices_src" && env \
  CPPFLAGS="$delegate_cppflags ${CPPFLAGS:-}" \
  LDFLAGS="$delegate_ldflags ${LDFLAGS:-}" \
  LIBS="$delegate_libs ${LIBS:-}" \
  ./configure "${configure_args[@]}" ${YICES2_CONFIGURE_FLAGS:-})

yices_mode="release"
tmp_yices_log="$(mktemp)"
$make_cmd -C "$yices_src" clean >/dev/null 2>&1 || true
if ! $make_cmd -C "$yices_src" "${make_parallel_args[@]}" lib 2> "$tmp_yices_log"; then
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
Libs: -L\${libdir} -lyices $delegate_libs $cudd_libs -lpoly -lgmp -lm
Cflags: -I\${includedir}
EOF

if [ "$platform" = "macos" ]; then
  if [ -f "$prefix/lib/libyices.2.dylib" ] && [ ! -f "$prefix/lib/libyices.dylib" ]; then
    ln -sf "libyices.2.dylib" "$prefix/lib/libyices.dylib"
  elif [ -f "$prefix/lib/libyices.dylib" ] && [ ! -f "$prefix/lib/libyices.2.dylib" ]; then
    ln -sf "libyices.dylib" "$prefix/lib/libyices.2.dylib"
  fi
elif [ "$platform" = "linux" ]; then
  ensure_linux_yices_links "$prefix/lib"
fi

if [ -n "$stamp" ]; then
  mkdir -p "$(dirname "$stamp")"
  printf '%s\n' "vendored yices installed" > "$stamp"
fi
