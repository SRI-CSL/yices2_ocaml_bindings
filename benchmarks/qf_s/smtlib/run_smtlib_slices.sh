#!/bin/sh
set -u

usage() {
  printf 'usage: %s active|frontier [slice]\n' "$0"
}

mode=${1:-active}
slice_filter=${2:-}

case "$mode" in
  active|frontier) ;;
  *)
    usage
    exit 2
    ;;
esac

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo_root=$(CDPATH= cd -- "$script_dir/../../.." && pwd)
manifest="$script_dir/$mode.tsv"
exe="$repo_root/_build/default/src_smt2/yices_string_smt2.exe"
default_smtlib_root="/Users/sgl/git/yices/SMTLib/non-incremental"
smtlib_root=${SMTLIB_ROOT:-$default_smtlib_root}
timeout_secs=${YICES_STRING_SMTLIB_TIMEOUT:-10}
strict_frontier=${YICES_STRING_FRONTIER_STRICT:-0}
require_smtlib=${YICES_STRING_REQUIRE_SMTLIB:-0}

if [ ! -d "$smtlib_root" ]; then
  printf 'SKIP SMT-LIB root not found: %s\n' "$smtlib_root"
  printf 'Set SMTLIB_ROOT to an unpacked SMT-LIB non-incremental directory.\n'
  if [ "$require_smtlib" = "1" ]; then
    exit 1
  fi
  exit 0
fi

if command -v timeout >/dev/null 2>&1; then
  timeout_bin=$(command -v timeout)
elif command -v gtimeout >/dev/null 2>&1; then
  timeout_bin=$(command -v gtimeout)
else
  timeout_bin=
fi

run_solver() {
  file=$1
  if [ -n "$timeout_bin" ]; then
    "$timeout_bin" "$timeout_secs" "$exe" "$file"
  else
    "$exe" "$file"
  fi
}

cd "$repo_root" || exit 1
dune build src_smt2/yices_string_smt2.exe >/dev/null || exit 1

total=0
passed=0
failed=0
locked=0
unlocked=0

while IFS= read -r line || [ -n "$line" ]; do
  case "$line" in
    ''|\#*) continue ;;
  esac

  set -- $line
  slice=$1
  case "$mode" in
    active)
      expected=$2
      target=$2
      rel=$3
      ;;
    frontier)
      expected=$2
      target=$3
      rel=$4
      ;;
  esac

  if [ -n "$slice_filter" ] && [ "$slice" != "$slice_filter" ]; then
    continue
  fi

  total=$((total + 1))
  file="$smtlib_root/$rel"
  if [ ! -f "$file" ]; then
    failed=$((failed + 1))
    printf 'FAIL missing benchmark: %s\n' "$file"
    continue
  fi

  if output=$(run_solver "$file" 2>&1); then
    actual=$(printf '%s\n' "$output" | sed -n '1p')
  else
    status=$?
    failed=$((failed + 1))
    first_line=$(printf '%s\n' "$output" | sed -n '1p')
    printf 'FAIL solver exited %-3d %-28s %s %s\n' "$status" "$slice" "$rel" "$first_line"
    continue
  fi

  case "$mode" in
    active)
      if [ "$actual" = "$expected" ]; then
        passed=$((passed + 1))
        printf 'PASS %-7s %-28s %s\n' "$actual" "$slice" "$rel"
      else
        failed=$((failed + 1))
        printf 'FAIL expected %-7s got %-7s %-28s %s\n' "$expected" "$actual" "$slice" "$rel"
      fi
      ;;
    frontier)
      if [ "$actual" = "$target" ]; then
        unlocked=$((unlocked + 1))
        printf 'UNLOCKED %-7s target=%-7s %-28s %s\n' "$actual" "$target" "$slice" "$rel"
      elif [ "$actual" = "$expected" ]; then
        locked=$((locked + 1))
        printf 'LOCKED   %-7s target=%-7s %-28s %s\n' "$actual" "$target" "$slice" "$rel"
      else
        failed=$((failed + 1))
        printf 'FAIL expected current %-7s or target %-7s got %-7s %-28s %s\n' \
          "$expected" "$target" "$actual" "$slice" "$rel"
      fi
      ;;
  esac
done < "$manifest"

if [ "$total" -eq 0 ]; then
  if [ -n "$slice_filter" ]; then
    printf 'No %s SMT-LIB entries matched slice %s.\n' "$mode" "$slice_filter"
    exit 1
  fi
  printf 'No %s SMT-LIB entries found in %s.\n' "$mode" "$manifest"
  exit 1
fi

case "$mode" in
  active)
    pass_rate=$((100 * passed / total))
    printf '\nSMT-LIB active summary: %d/%d expected statuses passed (%d%%).\n' \
      "$passed" "$total" "$pass_rate"
    ;;
  frontier)
    printf '\nSMT-LIB frontier summary: %d unlocked, %d locked, %d failed out of %d.\n' \
      "$unlocked" "$locked" "$failed" "$total"
    if [ "$strict_frontier" = "1" ] && [ "$locked" -ne 0 ]; then
      printf 'Frontier strict mode failed: %d locked rows remain.\n' "$locked"
      exit 1
    fi
    ;;
esac

if [ "$failed" -ne 0 ]; then
  exit 1
fi
