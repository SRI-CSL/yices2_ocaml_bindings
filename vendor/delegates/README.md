# Vendored Yices Delegate SAT Solvers

This directory contains the optional SAT delegate solvers used when the bindings build vendored Yices locally.

The solver sources are submodules under `src/`:

- `cadical`: `https://github.com/arminbiere/cadical`, commit `7b99c07f0bcab5824a5a3ce62c7066554017f641`
- `cryptominisat`: `https://github.com/BrunoDutertre/cryptominisat`, commit `b8c0e44beef01e52b5ce83ceed002728eebad92a`
- `kissat`: `https://github.com/BrunoDutertre/kissat`, commit `7e08e8703ad7b5575c35821988f328266a7534de`

`scripts/build_vendor_deps.sh` builds these into the same `vendor_install` prefix as CUDD and Yices, then configures Yices with the matching `HAVE_*` flags.

Set `YICES2_ENABLE_DELEGATES=0` to disable this part of the vendored build.
Set `YICES2_WITHOUT_DELEGATES=cadical,kissat` to omit only selected delegates.
