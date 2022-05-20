all:
	@dune build

test:
	@dune runtest

install:
	@dune install

uninstall:
	@dune uninstall

check: test

.PHONY: clean all check test install uninstall

clean:
	dune clean

publish%:
	opam publish --tag=v$* -v $* ocaml-community/cppo
