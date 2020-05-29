[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

# OCaml Bindings for Yices 2

This repository provides an ocaml library containing bindings for yices2's standard API (https://yices.csl.sri.com/doc/).
We provide two layers: of abstraction that wrap the yices C functions:
- a low level, where the functions that wrap the yices C functions are essentially identical to the yices API.
(+ some type safety provided by type abstraction over the types types_t of yices's types and term_t of yices's terms, which are now abstract instead of int32_t).
- a high level, which is more ocaml-friendly, using some ML datatype, etc.

## Building and Installing From Source

Technically you don't need yices to build and install the library: the yices binary code is not in the library itself.
But you should have yices for linking.

#### Using opam (needs 2.0 or higher, needs gmp)

In the directory of this `README.md`, build and install (in findlib) with the following command:
```
opam pin add yices2_bindings .
```

#### Without opam (gmp not mandatory, but it provides additional yices bindings)

The dependencies you need are listed in `src/META_gmp` (if you have gmp) or `src/META_nogmp` (if you do not have gmp). These are the findlib libraries that are / would be installed by the opam dependencies (listed in file `yices2_bindings.opam`), namely ocamlbuild, ctypes, ctypes-foreign, ppx_deriving, ppx_optcomp, sexplib, sexplib0, and, in presence of gmp, zarith, and ctypes-zarith.

To build, run the following command:
```
make
```
in the directory of this `README.md`. The build should automatically detect whether you have gmp and add the extra yices bindings if you do.

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

#### Building the documentation (this is broken at the moment, as ocamldoc does not seem to handle the latest ocaml features)

In the top-level directory, run the following command:
```
ocamlbuild -use-ocamlfind 'src/yices2_bindings.docdir/index.html'
```
You can then open `yices2_bindings.docdir/index.html` in a web browser.
