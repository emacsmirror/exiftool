EMACS = emacs

.PHONY: check
check:
	$(EMACS) -Q --batch -L . -l tests/exiftool-tests.el -f ert-run-tests-batch-and-exit
