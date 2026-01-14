#!/usr/bin/env bash
set -euo pipefail

tmp_log="$(mktemp)"
filtered_log=""
cleanup() {
  rm -f "${tmp_log}" "${filtered_log}"
}
trap cleanup EXIT

status=0
if ! dune uninstall 2> "${tmp_log}"; then
  status=$?
  if grep -q "following <package>\\.install are missing" "${tmp_log}"; then
    if dune build @install >/dev/null 2>&1; then
      if ! dune uninstall 2> "${tmp_log}"; then
        status=$?
      else
        status=0
      fi
    fi
  fi
fi

filtered_log="$(mktemp)"
grep -v -E "Directory .* is not empty|cannot delete \(ignoring\)\.|delete \(ignoring\)\.|cannot be installed because they do not exist|^Error: The following files which are listed in .*\.install|^Error: The following <package>\.install are missing|^- _build/install/|^- _build/.+\.install$|^Hint: try running \'dune build \\[\-p <pkg>\\] @install\'" "${tmp_log}" > "${filtered_log}" || true

if [ "$status" -ne 0 ]; then
  if [ -s "${filtered_log}" ]; then
    cat "${filtered_log}" >&2
    exit "$status"
  else
    exit 0
  fi
fi

cat "${filtered_log}" >&2 || true
