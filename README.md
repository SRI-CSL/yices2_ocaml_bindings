[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

# OCaml Bindings for Yices 2

This repository provides an ocaml library containing bindings for yices2's standard API (https://yices.csl.sri.com/doc/).
We provide two layers of abstraction that wrap the yices C functions:
- a low level, where the functions that wrap the yices C functions are essentially identical to the yices API.
(+ some type safety provided by type abstraction over the types types_t of yices's types and term_t of yices's terms, which are now abstract instead of int32_t).
- a high level, which is more ocaml-friendly, using some ML datatype, etc.

## Building and Installing From Source

#### Using opam (needs 2.0 or higher, needs gmp)

In the directory of this `README.md`, build and install (in findlib) with the following command:

```
opam install .
```
This expects the yices library (and the libraries it depends on) to be present in the relevant paths (like `/usr/local/lib`). If for some reason these libraries are not in the usual paths, you can specify their paths by setting 
the environment variables `LDFLAGS` (for the yices library) and `LD_LIBRARY_PATH` (for its dependencies, like libpoly or cudd), e.g.:

```
export LD_LIBRARY_PATH=[UNCONVENTIONAL_PATHS]:/usr/local/lib
export LDFLAGS="-L[UNCONVENTIONAL_PATH]"
```

#### Without opam (gmp not mandatory, but it provides additional yices bindings)

Besides Yices and its dependencies, the bindings need some OCaml dependencies, that are listed in `yices2_bindings.opam`. These are the findlib libraries that are / would be installed by opam, and that you can still install automatically with

```
opam install . --deps-only
```
These dependencies are namely: `ocamlbuild`, `ctypes`, `ctypes-foreign`, `ppx_deriving`, `ppx_optcomp`, `sexplib`, `sexplib0`, and, for gmp support, `zarith`, and `ctypes-zarith`.

To build, run the following command:
```
make
```
in the directory of this `README.md`. The build should automatically detect whether you have gmp and add the extra yices bindings if you do.
If for some weird reason you want the non-gmp version of the bindings even though you do have it, run this before `make`:

```
echo "[%%define gmp_present false]" > src/gmp.mlh
echo "[%%define gmp_present false]" > src_tests/gmp.mlh
```
and remove those two files to revert to automated detection.

To install (in findlib), run the following command:
```
make install
```

You can also use `make reinstall` and `make clean`.

#### Quick Testing (once findlib has the yices2_bindings library, whether it was installed manually or via opam)

In the directory of this `README.md`, run the following command:
```
make test
```
Whether the tests pass is rather self-explanatory.

Again, if the non-OCaml dependencies are not installed in conventional directories, make sure you set `LDFLAGS` and `LD_LIBRARY_PATH` correctly as described above.

#### Wrapping Yices as an SMTLib2 solver (experimental)

In the directory of this `README.md`, run the following command (after having installed the yices2_bindings library in findlib):
```
make smt2
```
This should produce an executable `yices_smt2` which you can run giving as sole argument the name of the SMTLib2 file to solve.
As with `make test`, this step involves linking and requires yices being installed. You can set `LDFLAGS` as above in case it is not in a standard location.

The code in `src_test` and in the `src_smt2/yices_smt2.ml` file give examples on how to use the bindings.

#### Building the documentation (this is broken at the moment, as ocamldoc does not seem to handle the latest ocaml features)

In the top-level directory, run the following command:

```
ocamlbuild -use-ocamlfind 'src/yices2_bindings.docdir/index.html'
```
You can then open `yices2_bindings.docdir/index.html` in a web browser.
