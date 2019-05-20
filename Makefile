EMACS = emacs

.PHONY: build check clean
build:
	$(EMACS) -Q --batch -L . --eval="(progn (setq byte-compile-debug t) (byte-recompile-directory \".\" 0))"

check:
	$(EMACS) -Q --batch -L . -l tests/exiftool-tests.el -f ert-run-tests-batch-and-exit

clean:
	rm -vf *.elc tests/*.elc
