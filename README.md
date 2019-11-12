[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

# OCampl Bindings for Yices 2 OCaml bindings

This repository provides an ocaml library containing bindings for yices2's standard API (https://yices.csl.sri.com/doc/).
We provide two layers: of abstraction that wrap the yices C functions:
- a low level, where the functions that wrap the yices C functions are essentially identical to thea yices API.
(+ some type safety provided by type abstraction over the types types_t of yices's types and term_t of yices's terms, which are now abstract instead of int32_t).
- a high level, which is more ocaml-friendly, using some ML datatype, etc

## Building From Source

#### Prerequisites

To build the ocaml bindings library from the source, you need the following OCaml dependencies (as in the opam file): ocaml, ocamlbuild, ocamlfind, ctypes, ctypes-foreign

Technically you don't need yices to build and install the library: the yices binary code is not in the library itself.
But you should have yices for linking.

#### Quick Installation (in findlib)

With opam (needs 2.0 or higher):
In the top-level directory, build and install with the following command:
```
opam pin add yices2_bindings .
```

With ocamlbuild directly:
Follow the `build` and `install` sections of the opam file; see also the `build.sh` script.

#### Quick Testing (once findlib has the yices2_bindings library)

In the top-level directory, build with the following command:
```
ocamlbuild -use-ocamlfind src_tests/yices_runtime.native
```

Run with the following command:
```yices_runtime.native```

You should get
```
First test, using exceptions for error handling
Initialising Yices version 2.6.1
Init done
New config done
Set config done
New context done
New param done
Set param done
SAT
Adding assertion "false"
UNSAT
Exited gracefully

Second test, using Result monad for error handling
Initialising Yices version 2.6.1
Init done
New config done
Set config done
New context done
New param done
Set param done
SAT
Adding assertion "false"
UNSAT
Exited gracefully
```

#### Building the documentation

In the top-level directory, do:
```
ocamlbuild -use-ocamlfind 'src/yices2_bindings.docdir/index.html'
```
You can then open `yices2_bindings.docdir/index.html` in a web browser.


#### Note

The file that immediately refers to yices's C API (located in `yices.h` of its distribution) is `src/yices_header.ml`.
This file has been generated automatically from `yices.h` by running the ctypes-of-clang ppx (https://github.com/ujamjar/ctypes_of_clang) on file `src/yices_header_src.ml`.
Editing the file, `src/yices_header.ml`, by hand is discouraged. If the yices API changes, 
then the suggested way of updating the bindings library is to 

1. Rerun ctypes_of_clang on `src/yices_header_src.ml`, then 
2. Edit the two levels of ocaml wrappings.

For running the ctypes-of-clang ppx, you need clang, and clang needs to find its standard libraries (stdio.h, stdint.h, etc) as well as yices.h.
Make sure of this by setting environment variables, e.g.
```
C_INCLUDE_PATH="/usr/lib/llvm-8/lib/clang/8.0.0/include/:$YICES_PATH/yices2/build/x86_64-pc-linux-gnu-release/dist/include/"
```
