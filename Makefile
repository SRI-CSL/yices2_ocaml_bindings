.PHONY: default build install uninstall reinstall test clean smt2 doc

default: build

doc:
	dune build @doc

build:
	dune build

smt2:
	dune build src_smt2

test: build
	dune build @runtest
	dune exec src_smt2/yices_smt2.exe -- src_smt2/simple.smt2

install: build
	dune install

reinstall: uninstall install

uninstall:
	dune uninstall

clean:
	dune clean
