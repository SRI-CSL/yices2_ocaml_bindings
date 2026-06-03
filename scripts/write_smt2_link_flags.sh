#!/usr/bin/env sh
set -eu

out="${1:-}"
if [ -z "$out" ]; then
  echo "usage: $0 <output.sexp>" >&2
  exit 2
fi

case "${YICES2_SMT2_STATIC:-0}" in
  1|yes|true|TRUE)
    vendor_lib=""
    for candidate in ../vendor_install/lib vendor_install/lib _build/default/vendor_install/lib; do
      if [ -d "$candidate" ]; then
        vendor_lib="$(cd "$candidate" && pwd)"
        break
      fi
    done

    flags='"-ccopt" "-static"'
    if [ -n "$vendor_lib" ]; then
      flags="$flags \"-ccopt\" \"-L$vendor_lib\" \"-ccopt\" \"-Wl,--start-group\" \"-cclib\" \"-lyices\""
      [ -f "$vendor_lib/libcadical.a" ] && flags="$flags \"-cclib\" \"-lcadical\""
      [ -f "$vendor_lib/libcryptominisat5.a" ] && flags="$flags \"-cclib\" \"-lcryptominisat5\""
      [ -f "$vendor_lib/libkissat.a" ] && flags="$flags \"-cclib\" \"-lkissat\""
      [ -f "$vendor_lib/libcudd.a" ] && flags="$flags \"-cclib\" \"-lcudd\""
      flags="$flags \"-ccopt\" \"-Wl,--end-group\""
      if [ -f "$vendor_lib/libcadical.a" ] || [ -f "$vendor_lib/libcryptominisat5.a" ] || [ -f "$vendor_lib/libkissat.a" ]; then
        flags="$flags \"-cclib\" \"-lstdc++\" \"-cclib\" \"-lm\""
      fi
      if [ -f "$vendor_lib/libcryptominisat5.a" ]; then
        flags="$flags \"-cclib\" \"-lz\" \"-cclib\" \"-pthread\""
      fi
    fi
    printf '(%s)\n' "$flags" > "$out"
    ;;
  *)
    printf '%s\n' '()' > "$out"
    ;;
esac
