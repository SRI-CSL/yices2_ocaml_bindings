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

# If the directory is empty, drop it quietly.
if [[ -z "$(find "${libdir}" -mindepth 1 -maxdepth 1 -print -quit)" ]]; then
  rmdir "${libdir}" 2>/dev/null || true
  exit 0
fi

# If it only contains empty directories, remove the whole tree.
if [[ -z "$(find "${libdir}" -type f -print -quit)" && -z "$(find "${libdir}" -type l -print -quit)" ]]; then
  rm -rf "${libdir}"
  exit 0
fi

# Only delete if the directory looks like a yices2 install. This avoids
# clobbering unrelated user-managed files if the path is re-used.
looks_like_yices2=0
if [[ -f "${libdir}/dune-package" ]]; then
  if grep -q "name yices2" "${libdir}/dune-package"; then
    looks_like_yices2=1
  fi
elif [[ -f "${libdir}/META" ]]; then
  if grep -q "Yices2 bindings" "${libdir}/META"; then
    looks_like_yices2=1
  fi
fi

if [[ "${looks_like_yices2}" -ne 1 ]]; then
  echo "Not removing ${libdir}; unable to confirm it belongs to yices2." >&2
  exit 0
fi

# Remove all artifacts installed under the package libdir.
rm -rf "${libdir}"
