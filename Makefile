.PHONY: build test clean utop fmt watch dev install doc server client

build:
	dune build

test:
	@if dune runtest; then \
		echo "✅✅✅ 😌 All is good in the hood -- tests are passing "; \
	else \
		echo "⛔⛔️⛔️️ 🤯 Some tests have failed."; \
		exit 1; \
	fi

watch:
	dune build -w

dev:
	dune build @all @runtest -w || :

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

# NOTE: this make target is a convenience artefact, relies on gnu-style arg-forwarding using --
# e.g. make server -- -p 5050
server client:
	@dune exec ./bin/main.exe $@ -- $(filter-out $@,$(MAKECMDGOALS))
