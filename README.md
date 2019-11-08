[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

# Yices 2 OCaml bindings

In development.

## Building From Source

#### Prerequisites

To build the ocaml bindings library from the source, you need:

- External deps: clang, yices
- OCaml dependencies (as in the opam file): ocaml, ocamlbuild, ocamlfind, ctypes, ctypes-foreign, ctypes-of-clang

To build the documentation, you also need: TBD

For compiling, clang needs to find its standard libraries (stdio.h, stdint.h, etc), as well as yices.h.
Make sure of this by setting environment variables, e.g.
```
C_INCLUDE_PATH="/usr/lib/llvm-8/lib/clang/8.0.0/include/:$YICES_PATH/yices2/build/x86_64-pc-linux-gnu-release/dist/include/"
```

#### Quick Installation (in findlib)

With opam:
In the top-level directory, do something like
```opam pin add yices2_bindings .```

With ocamlbuild directly:
Follow the build and install sections of the opam file.

### Quick Testing (once findlib has the yices2_bindings library)

In the top-level directory, do:
```
ocamlbuild -use-ocamlfind src_tests/yices_runtime.native
```

