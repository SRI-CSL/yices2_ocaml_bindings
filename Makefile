.PHONY: default build install uninstall reinstall test clean smt2

default: build

build:
	ocamlbuild -use-ocamlfind test_gmp src/yices2_bindings.cma src/yices2_bindings.cmxa src/META

smt2:
	ocamlbuild -use-ocamlfind src_smt2/yices_smt2.native

test:
	ocamlbuild -use-ocamlfind test_gmp src_tests/test.native
	./test.native

install: build
	ocamlfind install yices2_bindings _build/src/META\
          _build/src/yices2_header.cmx       _build/src/yices2_header.cmo       _build/src/yices2_header.cmi\
          _build/src/yices2_low_types.cmx    _build/src/yices2_low_types.cmo    _build/src/yices2_low_types.cmi\
          _build/src/yices2_high_types.cmx   _build/src/yices2_high_types.cmo   _build/src/yices2_high_types.cmi\
          _build/src/yices2_low.cmx          _build/src/yices2_low.cmo          _build/src/yices2_low.cmi\
          _build/src/yices2_high.cmx         _build/src/yices2_high.cmo         _build/src/yices2_high.cmi\
          _build/src/yices2_SMT2.cmx         _build/src/yices2_SMT2.cmo         _build/src/yices2_SMT2.cmi\
          _build/src/yices2_ext_bindings.cmx _build/src/yices2_ext_bindings.cmo _build/src/yices2_ext_bindings.cmi\
          _build/src/yices2_bindings.cma\
          _build/src/yices2_bindings.cmxa\
          _build/src/yices2_bindings.a

reinstall: uninstall install

uninstall:
	ocamlfind remove yices2_bindings

clean:
	ocamlbuild -clean
	git clean -dfXq
