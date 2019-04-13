EMACS=$(shell which emacs) -Q -batch -L .
ELS = \
  eiffel-mode.el \
  eiffel-mode-tests.el
ELCS = $(ELS:.el=.elc)

clean:
	rm -f $(ELCS)

cask: clean
	cask build

test: cask
	+ $(EMACS) -l eiffel-mode-tests.el -f ert-run-tests-batch-and-exit

# end
