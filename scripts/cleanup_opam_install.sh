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

libdir="${prefix}/lib/yices2"
if [[ ! -d "${libdir}" ]]; then
  exit 0
fi

unexpected="$(find "${libdir}" -mindepth 1 -maxdepth 1 \
  ! -name 'yices2.*' ! -name '__private__' -print)"

if [[ -n "${unexpected}" ]]; then
  echo "Not removing ${libdir}; unexpected entries present:" >&2
  echo "${unexpected}" >&2
  exit 1
fi

rm -f "${libdir}/yices2."*
rmdir "${libdir}/__private__" 2>/dev/null || true
rmdir "${libdir}" 2>/dev/null || true
