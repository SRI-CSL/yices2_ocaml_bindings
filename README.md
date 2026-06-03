[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

# OCaml Bindings for Yices 2

This repository provides an ocaml library containing bindings for yices2's standard API (https://yices.csl.sri.com/doc/).

## Contents

### Main library

The main OCaml library, called `yices2`, provides three levels of abstraction that wrap the yices C functions:

- Level **low**: the functions that wrap the yices C functions are essentially identical to the yices API.
(+ some type safety provided by type abstraction over the types types_t of yices's types and term_t of yices's terms, which are now abstract instead of int32_t).

- Level **high**: it is more ocaml-friendly, using some ML datatype (lists inside of C vectors, OCaml ints instead of C integer types), etc.

- Level **ext**: same as high, but it adds a few useful functions implemented on the OCaml side (e.g., computation of free variables, purification, log-keeping, pretty-printing functions in SMTLib2 format, etc).

At each level *lev*, the types and module signatures are defined in file *lev*_types.ml and the bindings are in *lev*.ml/mli

The library also provides an SMTLib2 parser for OCaml.

### Extension library

An additional OCaml library, called `yices2.extensions`, contains experimental extensions leveraging the bindings (e.g. tuple-blasting, etc).


### SMT-solver executable

Building the bindings will also compile an executable `yices_smt2.exe`. That executable file is an SMT-solver you can run on an SMTLib2 file such as `src_smt2/simple.smt2`. The solver plugs the SMTLib parser with the Yices2 API bindings, so it is essentially running Yices2, through the Yices2 API rather than throug the Yices2 native front-end for SMTLib2. It is mostly there as an example of how you can use the Yices2 bindings, and offers a quick test that everything works.

## Building and Installing

### Dependencies

Outside of the OCaml world, you need Yices2 compiled and installed, with the MCSAT mode enabled, which means you also need the Yices2/MCSAT dependencies [libpoly](https://github.com/SRI-CSL/libpoly) and [CUDD](https://github.com/ivmai/cudd). You also need gmp.

On the OCaml side, you need the [libpoly OCaml bindings](https://github.com/SRI-CSL/libpoly_ocaml_bindings) installed in findlib.
You also need `ctypes-zarith` installed from `git@github.com:SRI-CSL/ctypes-zarith.git` (the opam version is not compatible).
All of the other dependencies are listed in `yices2.opam`
and can be installed in findlib by opam (2.0 or higher), for instance 

```
opam install . --deps-only
```

If `pkg-config` cannot find an installed Yices2 with MCSAT enabled (checked via `yices_has_mcsat()`), the build will compile Yices2, CUDD, and the delegate SAT solvers from vendored submodules under `vendor/` and install into `_build/<context>/vendor_install` for the build.
Initialize these submodules before building:

```
git submodule update --init --recursive
```

You can pass extra Yices configure flags via `YICES2_CONFIGURE_FLAGS` if needed.
Build options are configured with `./configure`, which writes an ignored `config.mk` consumed by the Makefile. By default the build searches for a suitable system Yices first, requires MCSAT support, and enables all vendored delegates when a local Yices build is needed.

Examples:

```
./configure --local-yices
./configure --local-yices --no-mcsat
./configure --local-yices --without-delegate cadical --without-delegate kissat
./configure --local-yices --without-delegates=cadical,kissat
./configure --local-yices --static
```

Then use `make`, `make install`, `make test`, and the other Make targets normally. `--static` only affects the `yices_smt2.exe` executable, which is also run by `make test`, so the test target checks that the SMT2 executable starts and links correctly. To return to the default search behavior, run `./configure` without `--local-yices`.

When the vendored build runs, it installs Yices2, CUDD, and the delegate SAT solvers into `_build/<context>/vendor_install`. Builds will reuse that local install on subsequent `make` runs (no rebuild) as long as the directory is present. Running `dune install` (or `make install`) copies these into the current opam switch prefix (`opam var prefix`) so the switch stays clean if a build fails. To remove the opam-installed copies, run `make uninstall`.

### Building using opam (2.0 or higher)

In the directory of this `README.md`, build and install (in findlib) with the following command:

```
opam install .
```
This expects the yices library (and the libraries it depends on) to be present in the relevant paths (e.g., `/usr/local/lib`), as weel as its header files (e.g., `/usr/local/include/`). If for some reason these libraries are not in the usual paths, you can specify their paths by setting 
the environment variables `LDFLAGS` (for the yices library) and `LD_LIBRARY_PATH` (for its dependencies, like libpoly or cudd), 
as well as `C_INCLUDE_PATH`, e.g.:

```
export LD_LIBRARY_PATH=[UNCONVENTIONAL_PATHS]:/usr/local/lib
export LDFLAGS="-L[UNCONVENTIONAL_PATH]"
export C_INCLUDE_PATH="[UNCONVENTIONAL_PATH]"
```

### Building without opam

Assuming that the dependencies have been installed, you can build the yices2 bindings by running the following command:
```
make
```
in the directory of this `README.md`.

To install (in findlib), run the following command:
```
make install
```

You can also use `make reinstall` and `make clean`.

### Make targets

All commands run in the top-level directory of this repository.

- `make` / `make build`: build the OCaml libraries and executables, compiling vendored Yices/CUDD/delegates into `_build/<context>/vendor_install` if no suitable system Yices with MCSAT is found.
- `make install`: build, then install OCaml artifacts into the current opam switch and copy vendored Yices/CUDD/delegate artifacts into the opam prefix.
- `make uninstall`: uninstall OCaml artifacts and remove vendored Yices/CUDD/delegates from the opam prefix.
- `make reinstall`: uninstall then install.
- `make clean`: remove build artifacts under `_build`.
- `make test`: build and run the test suite plus a small SMT2 smoke test (sets `OCAML_DISABLE_ALTERNATE_SIGNAL_STACK=1` to avoid signal-stack teardown issues on some platforms).
- `make test-all`: run `make test`, then run a vendored-Yices MCSAT/free-variable stress check. The stress check fails immediately if it observes a stuck alternate-signal-stack state; it does not call `Yices.Global.exit`, so it does not rely on `yices_exit` or shutdown-time finalization to reveal the issue.
  Tune this stress check with `SIGALT_STRESS_ITERS=<n>` and `SIGALT_GC_INTERVAL=<n>`.
- `make smt2`: build the `yices_smt2.exe` SMT2 frontend.
- `make doc`: build API documentation under `_build/default/_doc/_html/index.html`.

### Quick Testing

In the directory of this `README.md`, run the following command:
```
make test
```
Whether the tests pass is rather self-explanatory.

Again, if the non-OCaml dependencies are not installed in conventional directories, make sure you set `LDFLAGS` and `LD_LIBRARY_PATH` correctly as described above.

You can also run the `yices_smt2.exe` executable, giving as sole argument the name of the SMTLib2 file to solve, suuch as `src_smt2/simple.smt2`.
As with `make test`, this step involves linking and requires yices being installed. You can set `LDFLAGS` as above in case it is not in a standard location.

The code in `src_test` and in the `src_smt2/yices_smt2.ml` file give examples on how to use the bindings.

### Building the documentation

In the top-level directory, run the following command:

```
make doc
```
You can then open `_build/default/_doc/_html/index.html` in a web browser.
