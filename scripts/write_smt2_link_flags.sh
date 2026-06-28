#!/usr/bin/env sh
set -eu

out="${1:-}"
if [ -z "$out" ]; then
  echo "usage: $0 <output.sexp>" >&2
  exit 2
fi

case "${YICES2_SMT2_STATIC:-0}" in
  1|yes|true|TRUE)
    case "$(uname -s 2>/dev/null || echo unknown)" in
      Darwin)
        # Darwin does not support fully static userland executables. Static
        # Yices/CUDD archive selection is handled by src_config/discover.ml.
        printf '%s\n' '()' > "$out"
        ;;
      *)
        printf '%s\n' '("-ccopt" "-static")' > "$out"
        ;;
    esac
    ;;
  *)
    printf '%s\n' '()' > "$out"
    ;;
esac
