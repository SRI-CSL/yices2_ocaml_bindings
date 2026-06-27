.PHONY: default build install uninstall reinstall test test-all test-sigalt-freevar test-string-benchmarks test-string-fmf test-string-smtlib-frontier clean smt2 doc with-local-yices

OPAM_SWITCH_PREFIX ?= $(shell opam var prefix 2>/dev/null)
SIGALT_STRESS_ITERS ?= 20000
SIGALT_GC_INTERVAL ?= 1000

-include config.mk

YICES2_FORCE_LOCAL ?= 0
YICES2_SMT2_STATIC ?= 0

DUNE_ENV = YICES2_FORCE_LOCAL=$(YICES2_FORCE_LOCAL) \
           YICES2_SMT2_STATIC=$(YICES2_SMT2_STATIC)
export OPAM_SWITCH_PREFIX

default: build

doc:
	$(DUNE_ENV) dune build @doc

build:
	@$(DUNE_ENV) dune build

with-local-yices:
	YICES2_FORCE_LOCAL=1 YICES2_SMT2_STATIC=$(YICES2_SMT2_STATIC) dune build src_smt2/yices_smt2.exe src_smt2/yices_string_smt2.exe

smt2:
	$(DUNE_ENV) dune build src_smt2

test: build
	@OCAML_DISABLE_ALTERNATE_SIGNAL_STACK=1 \
	$(DUNE_ENV) \
	DYLD_LIBRARY_PATH="$(OPAM_SWITCH_PREFIX)/lib:$(PWD)/_build/default/vendor_install/lib$${DYLD_LIBRARY_PATH:+:$${DYLD_LIBRARY_PATH}}" \
	LD_LIBRARY_PATH="$(OPAM_SWITCH_PREFIX)/lib:$(PWD)/_build/default/vendor_install/lib$${LD_LIBRARY_PATH:+:$${LD_LIBRARY_PATH}}" \
	dune build @runtest
	@OCAML_DISABLE_ALTERNATE_SIGNAL_STACK=1 \
	$(DUNE_ENV) \
	DYLD_LIBRARY_PATH="$(OPAM_SWITCH_PREFIX)/lib:$(PWD)/_build/default/vendor_install/lib$${DYLD_LIBRARY_PATH:+:$${DYLD_LIBRARY_PATH}}" \
	LD_LIBRARY_PATH="$(OPAM_SWITCH_PREFIX)/lib:$(PWD)/_build/default/vendor_install/lib$${LD_LIBRARY_PATH:+:$${LD_LIBRARY_PATH}}" \
	dune exec src_smt2/yices_smt2.exe -- src_smt2/simple.smt2
	@OCAML_DISABLE_ALTERNATE_SIGNAL_STACK=1 \
	$(DUNE_ENV) \
	DYLD_LIBRARY_PATH="$(OPAM_SWITCH_PREFIX)/lib:$(PWD)/_build/default/vendor_install/lib$${DYLD_LIBRARY_PATH:+:$${DYLD_LIBRARY_PATH}}" \
	LD_LIBRARY_PATH="$(OPAM_SWITCH_PREFIX)/lib:$(PWD)/_build/default/vendor_install/lib$${LD_LIBRARY_PATH:+:$${LD_LIBRARY_PATH}}" \
	dune exec src_smt2/yices_smt2.exe -- src_smt2/qf_nra_tuples_sat.smt2
	@OCAML_DISABLE_ALTERNATE_SIGNAL_STACK=1 \
	$(DUNE_ENV) \
	DYLD_LIBRARY_PATH="$(OPAM_SWITCH_PREFIX)/lib:$(PWD)/_build/default/vendor_install/lib$${DYLD_LIBRARY_PATH:+:$${DYLD_LIBRARY_PATH}}" \
	LD_LIBRARY_PATH="$(OPAM_SWITCH_PREFIX)/lib:$(PWD)/_build/default/vendor_install/lib$${LD_LIBRARY_PATH:+:$${LD_LIBRARY_PATH}}" \
	dune exec src_smt2/yices_smt2.exe -- src_smt2/qf_nra_tuples_unsat.smt2

test-all: test test-sigalt-freevar

test-string-benchmarks:
	@$(DUNE_ENV) benchmarks/qf_s/run_core.sh
	@$(DUNE_ENV) benchmarks/qf_s/run_extended.sh
	@$(DUNE_ENV) benchmarks/qf_s/run_fmf.sh
	@$(DUNE_ENV) benchmarks/qf_s/smtlib/run_smtlib_slices.sh active

test-string-fmf:
	@$(DUNE_ENV) benchmarks/qf_s/run_fmf.sh

test-string-smtlib-frontier:
	@$(DUNE_ENV) benchmarks/qf_s/smtlib/run_smtlib_slices.sh frontier

test-sigalt-freevar:
	@printf "Running sigalt free-variable stress (%s iterations, GC every %s)\n" "$(SIGALT_STRESS_ITERS)" "$(SIGALT_GC_INTERVAL)"
	@YICES2_FORCE_LOCAL=1 \
	YICES2_SMT2_STATIC=$(YICES2_SMT2_STATIC) \
	YICES_SIGALT_STRESS_ITERS=$(SIGALT_STRESS_ITERS) \
	YICES_SIGALT_GC_INTERVAL=$(SIGALT_GC_INTERVAL) \
	DYLD_LIBRARY_PATH="$(OPAM_SWITCH_PREFIX)/lib:$(PWD)/_build/default/vendor_install/lib$${DYLD_LIBRARY_PATH:+:$${DYLD_LIBRARY_PATH}}" \
	LD_LIBRARY_PATH="$(OPAM_SWITCH_PREFIX)/lib:$(PWD)/_build/default/vendor_install/lib$${LD_LIBRARY_PATH:+:$${LD_LIBRARY_PATH}}" \
	dune exec src_tests/sigalt_freevar_stress.exe

install: build
	$(DUNE_ENV) dune build @install
	$(DUNE_ENV) dune install
	./scripts/install_vendor_deps.sh --from-prefix _build/default/vendor_install

reinstall: uninstall install

uninstall:
	./scripts/dune_uninstall_quiet.sh
	./scripts/cleanup_opam_install.sh
	./scripts/uninstall_vendor_deps.sh

clean:
	dune clean
