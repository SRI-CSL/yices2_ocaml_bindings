#!/bin/sh
set -u

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo_root=$(CDPATH= cd -- "$script_dir/../.." && pwd)
bench_dir="$script_dir/fmf"
exe="$repo_root/_build/default/src_smt2/yices_string_smt2.exe"

cd "$repo_root" || exit 1
dune build src_smt2/yices_string_smt2.exe >/dev/null || exit 1

total=0
passed=0
failed=0

files=$(find "$bench_dir" -type f -name '*.smt2' -print | sort)

for file in $files; do
  total=$((total + 1))
  expected=$(sed -n 's/^; EXPECT: //p' "$file" | head -n 1)
  max_total=$(sed -n 's/^; FMF_MAX_TOTAL_LENGTH: //p' "$file" | head -n 1)
  max_rounds=$(sed -n 's/^; FMF_MAX_ROUNDS: //p' "$file" | head -n 1)
  trace=$(sed -n 's/^; FMF_EXPECT_TRACE: //p' "$file" | head -n 1)

  if [ -z "$expected" ] || [ -z "$max_total" ] || [ -z "$max_rounds" ] || [ -z "$trace" ]; then
    printf 'FAIL missing FMF metadata: %s\n' "$file"
    failed=$((failed + 1))
    continue
  fi

  err_file=$(mktemp "${TMPDIR:-/tmp}/yices-string-fmf.XXXXXX") || exit 1
  if output=$(YICES_STRING_FMF=1 \
      YICES_STRING_FMF_LOG=1 \
      YICES_STRING_FMF_MAX_TOTAL_LENGTH="$max_total" \
      YICES_STRING_FMF_MAX_ROUNDS="$max_rounds" \
      "$exe" "$file" 2>"$err_file"); then
    actual=$(printf '%s\n' "$output" | sed -n '1p')
  else
    status=$?
    printf 'FAIL solver exited %d: %s\n' "$status" "$file"
    rm -f "$err_file"
    failed=$((failed + 1))
    continue
  fi

  if [ "$actual" = "$expected" ] && grep -F "$trace" "$err_file" >/dev/null 2>&1; then
    passed=$((passed + 1))
    printf 'PASS %-7s %s\n' "$actual" "${file#$repo_root/}"
  else
    failed=$((failed + 1))
    printf 'FAIL expected %-7s got %-7s %s\n' "$expected" "$actual" "${file#$repo_root/}"
    printf '     expected trace: %s\n' "$trace"
    printf '     actual trace:\n'
    sed 's/^/       /' "$err_file"
  fi
  rm -f "$err_file"
done

if [ "$total" -eq 0 ]; then
  printf 'No FMF benchmarks found in %s\n' "$bench_dir"
  exit 1
fi

pass_rate=$((100 * passed / total))
printf '\nStage A6 FMF summary: %d/%d expected statuses and traces passed (%d%%).\n' \
  "$passed" "$total" "$pass_rate"

if [ "$failed" -ne 0 ]; then
  exit 1
fi
