# SMT-LIB QF_S/QF_SLIA Slices

This directory tracks original SMT-LIB string benchmarks without copying them
into the repository. The runner reads files from:

```sh
SMTLIB_ROOT=/Users/sgl/git/yices/SMTLib/non-incremental
```

Set `SMTLIB_ROOT` to another unpacked SMT-LIB `non-incremental` directory if
needed. If the root is missing, the runner skips by default; set
`YICES_STRING_REQUIRE_SMTLIB=1` to make that a failure.

## Manifests

- `active.tsv` is a regression gate. Each row has
  `slice expected relative_path`, and `make test-string-benchmarks` runs these
  cases when `SMTLIB_ROOT` is available.
- `frontier.tsv` is the unlock queue. Each row has
  `slice current target relative_path`, where `current` is the current solver
  result and `target` is the SMT-LIB `:status`.

The frontier is deliberately organized in groups of about six benchmarks. For a
new string-solver slice, run:

```sh
benchmarks/qf_s/smtlib/run_smtlib_slices.sh frontier <slice-name>
```

When an implementation changes those rows from `LOCKED` to `UNLOCKED`, move the
unlocked rows to `active.tsv` with the target status as `expected`, and add a
new frontier group for the next increment. Use
`YICES_STRING_FRONTIER_STRICT=1` to make locked frontier rows fail while
validating a completed slice.

The current frontier groups are:

- `stage3_contains_unsat_matching`: negative/contradictory `str.contains`
  matching cases that still need stronger non-containment reasoning.
- `stage3_indexof_matching`: `str.indexof` matching contradictions.
- `stage3_word_equation_regex`: word-equation cases with regex side
  constraints.

The current active groups are:

- `stage2_word_equation_active`: original Woorpje word-equation benchmarks
  covered by the concat/length solver foundation.
- `stage3_regex_active`: original Automatark regex benchmarks covered by the
  Stage 3 regex subset.
- `stage3_contains_witness_active`: original matching SAT benchmarks covered by
  the positive `str.contains` witness split.
- `stage3_indexof_matching_active`: original matching/indexof contradiction
  rows already solved by the guarded symbolic `indexof` reduction.
- `stage3_ext_rewrite_active`: Noetzli rewrite-verification benchmarks over
  `contains`, `at`, and `substr`. Their SMT-LIB metadata says `unknown`, so
  these rows are current-result regression checks rather than official-status
  target checks.
