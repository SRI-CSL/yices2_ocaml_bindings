.PHONY: default build install uninstall reinstall test clean smt2

default: build

build:
	dune build

smt2:
	dune build src_smt2

test:
	dune build
	dune build @runtest

install: build
	dune install

reinstall: uninstall install

uninstall:
	dune uninstall

clean:
	dune clean
