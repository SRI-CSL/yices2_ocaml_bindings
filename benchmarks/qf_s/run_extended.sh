#!/bin/sh
set -u

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo_root=$(CDPATH= cd -- "$script_dir/../.." && pwd)
bench_dir="$script_dir/extended"
exe="$repo_root/_build/default/src_smt2/yices_string_smt2.exe"

cd "$repo_root" || exit 1
dune build src_smt2/yices_string_smt2.exe >/dev/null || exit 1

total=0
passed=0
solved=0
failed=0

files=$(find "$bench_dir" -type f -name '*.smt2' -print | sort)

for file in $files; do
  total=$((total + 1))
  expected=$(sed -n 's/^; EXPECT: //p' "$file" | head -n 1)
  if [ -z "$expected" ]; then
    printf 'FAIL missing EXPECT: %s\n' "$file"
    failed=$((failed + 1))
    continue
  fi

  if output=$("$exe" "$file"); then
    actual=$(printf '%s\n' "$output" | sed -n '1p')
  else
    status=$?
    printf 'FAIL solver exited %d: %s\n' "$status" "$file"
    failed=$((failed + 1))
    continue
  fi

  if [ "$actual" = "$expected" ]; then
    passed=$((passed + 1))
    case "$actual" in
      sat|unsat) solved=$((solved + 1)) ;;
    esac
    printf 'PASS %-7s %s\n' "$actual" "${file#$repo_root/}"
  else
    failed=$((failed + 1))
    printf 'FAIL expected %-7s got %-7s %s\n' "$expected" "$actual" "${file#$repo_root/}"
  fi
done

if [ "$total" -eq 0 ]; then
  printf 'No benchmarks found in %s\n' "$bench_dir"
  exit 1
fi

pass_rate=$((100 * passed / total))
solve_rate=$((100 * solved / total))
printf '\nStage 3 extended summary: %d/%d expected statuses passed (%d%%); %d/%d solved as sat/unsat (%d%%).\n' \
  "$passed" "$total" "$pass_rate" "$solved" "$total" "$solve_rate"

if [ "$failed" -ne 0 ]; then
  exit 1
fi
