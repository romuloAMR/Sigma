CABAL = cabal

.PHONY: help install info build run play clean test problem-1 problem-2 problem-3 problem-4

.DEFAULT_GOAL := help

help: ## Show available commands
	@echo "Available commands for Sigma project:"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-10s\033[0m %s\n", $$1, $$2}'

install: ## Install dependencies and highlighting
	rm -rf ~/.vscode-server/extensions/sigma-lang  && \
	cp -r sigma-lang/ ~/.vscode-server/extensions/  && \
	cabal update && \
	cabal install alex happy && \
	make info

info: ## Display versions of core components
	@echo "--- Environment Versions ---"
	@ghc --version
	@$(CABAL) --version
	@cat /etc/os-release

build: ## Compile the project using cabal
	@echo "Building the project..."
	$(CABAL) build

run: ## Compile and execute the program
	@echo "Running Sigma..."
	@$(CABAL) run sigma

play: ## Open the interactive shell (REPL) with project context
	@echo "Starting REPL (type :q to quit)..."
	$(CABAL) repl

clean: ## Remove build artifacts
	@echo "Cleaning build directories..."
	$(CABAL) clean

PROBLEMS_DIR = problems

test: ## Merge Sort Test
	cabal run sigma < $(PROBLEMS_DIR)/test.sg

problem-1: ## Run Problem 1
	cabal run sigma -- $(PROBLEMS_DIR)/problem1.sg

problem-2: ## Run Problem 2
	cabal run sigma -- $(PROBLEMS_DIR)/problem2.sg

problem-3: ## Run Problem 3
	cabal run sigma -- $(PROBLEMS_DIR)/problem3.sg

problem-4: ## Run Problem 4
	cabal run sigma -- $(PROBLEMS_DIR)/problem4.sg
