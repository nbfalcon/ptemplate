build:
	cask build
all: build

clean:
	$(RM) *.elc

test:
	cask exec ert-runner
check: test

.PHONY: all build clean test
