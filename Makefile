CABAL = cabal

.PHONY: help info build run play clean

.DEFAULT_GOAL := help

help: ## Show available commands
	@echo "Available commands for Sigma project:"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-10s\033[0m %s\n", $$1, $$2}'

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

test: ## Merge Sort Test
	cabal run sigma < test.sg

problem-1: ## Run Problem 1
	cabal run sigma < problem1.sg

problem-2: ## Run Problem 2
	cabal run sigma < problem2.sg

problem-3: ## Run Problem 3
	cabal run sigma < problem3.sg
