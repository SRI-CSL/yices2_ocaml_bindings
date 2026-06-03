#!/usr/bin/env bash
set -euo pipefail

prefix="${OPAM_SWITCH_PREFIX:-}"
if [[ -z "${prefix}" ]]; then
  if command -v opam >/dev/null 2>&1; then
    prefix="$(opam var prefix)"
  fi
fi

if [[ -z "${prefix}" ]]; then
  echo "Unable to determine opam prefix (OPAM_SWITCH_PREFIX not set and opam not found)." >&2
  exit 1
fi

echo "Removing vendored Yices/CUDD/delegates from ${prefix}"

rm -f \
  "${prefix}/lib/libyices."* \
  "${prefix}/lib/libcudd."* \
  "${prefix}/lib/libcadical."* \
  "${prefix}/lib/libcryptominisat5."* \
  "${prefix}/lib/libkissat."* \
  "${prefix}/lib/pkgconfig/yices.pc" \
  "${prefix}/include/yices.h" \
  "${prefix}/include/yices_types.h" \
  "${prefix}/include/yices_exit_codes.h" \
  "${prefix}/include/yices_limits.h" \
  "${prefix}/include/cudd.h" \
  "${prefix}/include/ccadical.h" \
  "${prefix}/include/kissat.h"

rm -rf "${prefix}/include/cryptominisat5"
