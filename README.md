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

On the OCaml side, you need the [libpoly OCaml bindings](https://github.com/SRI-CSL/yices2_ocaml_bindings) installed in findlib.
You also need `ctypes-zarith` installed from `git@github.com:SRI-CSL/ctypes-zarith.git` (the opam version is not compatible).
All of the other dependencies are listed in `yices2.opam`
and can be installed in findlib by opam (2.0 or higher), for instance 

```
opam install . --deps-only
```

If `pkg-config` cannot find an installed Yices2, the build will try to compile Yices2 and CUDD from vendored submodules under `vendor/` and install into `_build/<context>/vendor_install` for the build.
Initialize these submodules before building:

```
git submodule update --init --recursive
```

You can pass extra Yices configure flags via `YICES2_CONFIGURE_FLAGS` if needed.
If you want to force the vendored build even when a system Yices is present, run:

```
make with-local-yices
```

When the vendored build runs, it installs Yices2 and CUDD into `_build/<context>/vendor_install`. Running `dune install` (or `make install`) copies these into the current opam switch prefix (`opam var prefix`) so the switch stays clean if a build fails.

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
