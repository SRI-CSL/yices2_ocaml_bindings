#!/usr/bin/env sh
set -eu

out="${1:-}"
if [ -z "$out" ]; then
  echo "usage: $0 <output.sexp>" >&2
  exit 2
fi

case "${YICES2_SMT2_STATIC:-0}" in
  1|yes|true|TRUE)
    printf '%s\n' '("-ccopt" "-static")' > "$out"
    ;;
  *)
    printf '%s\n' '()' > "$out"
    ;;
esac
