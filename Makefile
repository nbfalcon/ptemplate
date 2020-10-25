all: build
build: ptemplate.el
	cask build

clean:
	$(RM) *.elc

check:
	cask exec ert-runner
test: check

.PHONY: check clean
