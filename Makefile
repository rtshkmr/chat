.PHONY: help setup install-deps build install test watch dev fmt utop doc server client clean

# NOTE: [AI-USAGE]: makefile was jazzed up by Claude Haiku 4.5

OCAML_VERSION ?= 5.4.1
BOLD := \033[1m
GREEN := \033[32m
RESET := \033[0m

help:
	@echo "$(BOLD)ChatTCP - Makefile$(RESET)"
	@echo ""
	@echo "$(GREEN)Initial Setup:$(RESET)"
	@echo "  make setup          Create opam switch and install dependencies"
	@echo ""
	@echo "$(GREEN)Build & Install:$(RESET)"
	@echo "  make build          Build the executable"
	@echo "  make install        Install executable to opam bin/ directory"
	@echo ""
	@echo "$(GREEN)Run:$(RESET)"
	@echo "  make server [ARGS]  Run server (e.g. make server -- -p 5050)"
	@echo "  make client [ARGS]  Run client (e.g. make client -- --host localhost)"
	@echo ""
	@echo "  Have to use the Gnu arg-forwarder syntax, the -- is necessary to interop with makefile"
	@echo ""
	@echo "$(GREEN)Development:$(RESET)"
	@echo "  make test           Run tests"
	@echo "  make watch          Watch-mode rebuild"
	@echo "  make dev            Watch + test"
	@echo "  make fmt            Format code"
	@echo "  make utop           Interactive REPL"
	@echo ""
	@echo "$(GREEN)Cleanup:$(RESET)"
	@echo "  make clean          Remove build artifacts"

setup:
	@echo "$(GREEN)Setting up OCaml environment...$(RESET)"
	@if [ ! -f dune-project ]; then \
		echo "Error: dune-project not found"; \
		exit 1; \
	fi
	opam switch create . $(OCAML_VERSION) --deps-only --with-test --with-dev -y 2>/dev/null || opam install . --deps-only --with-test --with-dev -y
	@echo ""
	@echo "$(GREEN)✅ Setup complete!$(RESET)"
	@echo "$(BOLD)Next steps:$(RESET)"
	@echo "  1. Activate the switch:"
	@echo "     eval \$$(opam env)"
	@echo "  2. Build and install:"
	@echo "     make build && make install"
	@echo "  3. Run your chat server:"
	@echo "     make server -- -p 5050"

install-deps:
	@echo "$(GREEN)Installing dependencies...$(RESET)"
	opam install . --deps-only -y
	@echo "$(GREEN)✅ Dependencies installed$(RESET)"


build:
	@echo "$(GREEN)Building executable...$(RESET)"
	dune build
	@echo "$(GREEN)✅ Built: ./_build/default/bin/chat.exe$(RESET)"

install: build
	@echo "$(GREEN)Installing to opam environment...$(RESET)"
	dune install
	@echo "$(GREEN)✅ Installed! You can now run 'chat' directly$(RESET)"
	@echo "   chat server -p 5050 # or just chat server works on default args"
	@echo "   chat client --host localhost -p 5050 # or just chat client works on default args"

_CHAT_EXE := ./_build/default/bin/main.exe
server: build
	@$(if $(wildcard $(_CHAT_EXE)), \
		$(_CHAT_EXE) server $(filter-out $@,$(MAKECMDGOALS)), \
		echo "Error: executable not found. Run 'make build' first")

client: build
	@$(if $(wildcard $(_CHAT_EXE)), \
		$(_CHAT_EXE) client $(filter-out $@,$(MAKECMDGOALS)), \
		echo "Error: executable not found. Run 'make build' first")

test:
	@echo "$(GREEN)Running tests...$(RESET)"
	@if dune runtest -j auto; then \
		echo "$(GREEN)✅ All is good in the hood; all tests passing$(RESET)"; \
	else \
		echo "$(BOLD)⛔ Tests failed 🫡 $(RESET)"; \
		exit 1; \
	fi


nuketest: clean
	@echo "$(GREEN)Nuclear rebuild + test...$(RESET)"
	@if make build && dune runtest -j auto; then \
		echo "$(GREEN)✅ Nuclear tests passing$(RESET)"; \
	else \
		echo "$(BOLD)⛔ Tests failed$(RESET)"; \
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
	@echo "$(GREEN)✅ Cleaned build artifacts$(RESET)"

fmt:
	dune fmt
	@echo "$(GREEN)✅ Code formatted$(RESET)"

doc:
	dune build @doc
	@echo "$(GREEN)✅ Documentation built in _build/default/_doc/$(RESET)"

# Allow extra args
%:
	@:
