# Stage 3 Extended QF_S Benchmarks

This directory is the Stage 3 regression baseline for selected extended string
operations and the initial regex subset.

Each `.smt2` file starts with `; EXPECT: <status>`. Run the suite with:

```sh
benchmarks/qf_s/run_extended.sh
```

The suite is intentionally small. Most cases are concrete-heavy; the symbolic
positive-`contains`, symbolic `substr`, symbolic `indexof`, and symbolic
`replace` cases check the shared witness splits used by Stage 3. The suite
checks the Stage 3 contract that extended terms are validated against the
concrete model and that violated extended terms produce semantic or bounded
symbolic refinement lemmas.

Original SMT-LIB benchmarks are tracked separately in `../smtlib/`. That
manifest-based suite keeps a small active regression baseline and a frontier of
roughly half-dozen-benchmark slices to promote as new solver increments land.
