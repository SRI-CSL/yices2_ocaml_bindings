.PHONY: default build install uninstall reinstall test clean smt2 doc with-local-yices

OPAM_SWITCH_PREFIX ?= $(shell opam var prefix 2>/dev/null)
export OPAM_SWITCH_PREFIX

default: build

doc:
	dune build @doc

build:
	dune build

with-local-yices:
	YICES2_FORCE_LOCAL=1 dune build

smt2:
	dune build src_smt2

test: build
	OCAML_DISABLE_ALTERNATE_SIGNAL_STACK=1 \
	DYLD_LIBRARY_PATH="$(OPAM_SWITCH_PREFIX)/lib:$(PWD)/_build/default/vendor_install/lib$${DYLD_LIBRARY_PATH:+:$${DYLD_LIBRARY_PATH}}" \
	LD_LIBRARY_PATH="$(OPAM_SWITCH_PREFIX)/lib:$(PWD)/_build/default/vendor_install/lib$${LD_LIBRARY_PATH:+:$${LD_LIBRARY_PATH}}" \
	dune build @runtest
	OCAML_DISABLE_ALTERNATE_SIGNAL_STACK=1 \
	DYLD_LIBRARY_PATH="$(OPAM_SWITCH_PREFIX)/lib:$(PWD)/_build/default/vendor_install/lib$${DYLD_LIBRARY_PATH:+:$${DYLD_LIBRARY_PATH}}" \
	LD_LIBRARY_PATH="$(OPAM_SWITCH_PREFIX)/lib:$(PWD)/_build/default/vendor_install/lib$${LD_LIBRARY_PATH:+:$${LD_LIBRARY_PATH}}" \
	dune exec src_smt2/yices_smt2.exe -- src_smt2/simple.smt2

install: build
	dune build @install
	dune install
	./scripts/install_vendor_deps.sh --from-prefix _build/default/vendor_install

reinstall: uninstall install

uninstall:
	./scripts/dune_uninstall_quiet.sh
	./scripts/cleanup_opam_install.sh
	./scripts/uninstall_vendor_deps.sh

clean:
	dune clean
