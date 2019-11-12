[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

# Yices 2 OCaml bindings

An ocaml library containing bindings for yices2's standard API (https://yices.csl.sri.com/doc/), with two levels of abstraction that wrap the yices C functions:
- a low level, where the functions are really as in the yices API
(+ some type safety provided by type abstraction over the types types_t of yices's types and term_t of yices's terms, which are now abstract instead of int32_t).
- a high level, which is more ocaml-friendly, using some ML datatype, etc

## Building From Source

#### Prerequisites

To build the ocaml bindings library from the source, you need the following OCaml dependencies (as in the opam file): ocaml, ocamlbuild, ocamlfind, ctypes, ctypes-foreign

To build the documentation, you also need: TBD

Technically you don't need yices to build and install the library: the yices binary code is not in the library itself.
But you should have yices for linking.

#### Quick Installation (in findlib)

With opam (needs 2.0 or higher):
In the top-level directory, do something like
```opam pin add yices2_bindings .```

With ocamlbuild directly:
Follow the `build` and `install` sections of the opam file, and the `build.sh` script.

#### Quick Testing (once findlib has the yices2_bindings library)

In the top-level directory, do:
```
ocamlbuild -use-ocamlfind src_tests/yices_runtime.native
```

#### Building the documentation

In the top-level directory, do:
```
ocamlbuild -use-ocamlfind 'src/yices2_bindings.docdir/index.html'
```

#### Note

The file that immediately refers to yices's C API (in `yices.h` of its distribution) is `src/yices_header.ml`.
This file has been generated automatically from `yices.h` by running the ctypes-of-clang ppx (https://github.com/ujamjar/ctypes_of_clang) on file `src/yices_header_src.ml`.
It is discouraged to edit it by hand `src/yices_header.ml`. If the yices API changes, the advised way of updating this library is to start by re-running ctypes_of_clang on `src/yices_header_src.ml` and then edit the two levels of ocaml wrappings.

For running the ctypes-of-clang ppx, you need clang, and clang needs to find its standard libraries (stdio.h, stdint.h, etc) as well as yices.h.
Make sure of this by setting environment variables, e.g.
```
C_INCLUDE_PATH="/usr/lib/llvm-8/lib/clang/8.0.0/include/:$YICES_PATH/yices2/build/x86_64-pc-linux-gnu-release/dist/include/"
```
