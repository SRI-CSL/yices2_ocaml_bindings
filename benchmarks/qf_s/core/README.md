# Stage 2 Core QF_S Benchmarks

This directory is a curated regression suite for the Stage 2 OCaml string
extension. It intentionally targets the fragment implemented in Stage 2:

- string literals;
- string variables;
- concatenation;
- length;
- equality and disequality;
- one-unknown concat/literal refinements;
- conservative `unknown` for unsupported multi-unknown word equations.

Each `.smt2` file starts with `; EXPECT: <status>`. Run the suite with:

```sh
benchmarks/qf_s/run_core.sh
```

The full SMT-LIB string archives are extracted outside this repository at
`/Users/sgl/git/yices/SMTLib/non-incremental/`. This core suite is small on
purpose: it is a stable Stage 2 baseline before broader SMT-LIB sampling and
Stage 3 extended string functions.
