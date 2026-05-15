.PHONY: build test clean utop fmt watch dev install doc run

build:
	dune build

test:
	dune runtest

watch:
	dune build -w

dev:
	dune build @all @runtest -w

utop:
	dune utop

clean:
	dune clean

fmt:
	dune fmt

install:
	dune build @install

doc:
	dune build @doc

run:
	dune exec ./bin/main.exe -- $(filter-out $@,$(MAKECMDGOALS))
