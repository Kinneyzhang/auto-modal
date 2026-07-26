EMACS ?= emacs
BATCH = $(EMACS) --batch -Q -L .

ELS = auto-modal.el auto-modal-config.el
ELCS = $(ELS:.el=.elc)

.PHONY: all compile test checkdoc lint clean

all: lint test

compile: clean
	$(BATCH) --eval "(setq byte-compile-error-on-warn t)" \
		-f batch-byte-compile $(ELS)

test:
	$(BATCH) -l auto-modal-tests.el -f ert-run-tests-batch-and-exit

checkdoc:
	$(BATCH) --eval "(progn \
	  (require 'checkdoc) \
	  (setq sentence-end-double-space nil) \
	  (checkdoc-file \"auto-modal.el\") \
	  (checkdoc-file \"auto-modal-config.el\"))"

lint: compile checkdoc

clean:
	rm -f $(ELCS)
