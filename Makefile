EMACS ?= emacs
GRAMMAR_DIR ?= $(CURDIR)/.cache/tree-sitter

SOURCES := crates.el crates-db.el crates-semver.el crates-toml.el
TESTS := $(wildcard test/*-test.el)

BATCH = $(EMACS) -Q --batch -L . -L test \
	--eval '(setq load-prefer-newer t treesit-extra-load-path (list "$(GRAMMAR_DIR)"))'

.PHONY: all deps compile checkdoc lint test clean

all: compile checkdoc lint test

deps:
	$(BATCH) -l test/make.el -f crates-make-deps

compile:
	$(BATCH) --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile $(SOURCES)

checkdoc:
	$(BATCH) -l test/make.el -f crates-make-checkdoc

lint:
	$(BATCH) -l test/make.el -f crates-make-lint

test:
	$(BATCH) $(addprefix -l ,$(TESTS)) -f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc test/*.elc
