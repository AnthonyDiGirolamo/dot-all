SHELL=bash

# First rule (help) is the default if make is run with no targets
.PHONY: help
help:  ## show help
	@grep -hE '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
# Help grep source: https://marmelab.com/blog/2016/02/29/auto-documented-makefile.html

CACHEDIR := ./.cache

FISH_URL := https://github.com/fish-shell/fish-shell/releases/download/3.1.0/fish-3.1.0.tar.gz
EMACS_URL := http://ftpmirror.gnu.org/emacs/emacs-26.3.tar.xz

orgs := $(filter-out $(docs), $(wildcard *.org))

all: tangle

clean:
	rm -rf $(CACHEDIR)

tangle: $(orgs)  ## tangle all dotfiles
	@echo -n "[$<] "
	@emacs -Q --batch --eval "(progn (require 'org) (require 'ob-shell) (setq org-confirm-babel-evaluate nil) (org-babel-tangle-file \"$<\"))"

.ONESHELL: install-fish
.PHONY: install-fish
install-fish:  ## download, compile and install fish
	mkdir -p $(CACHEDIR)/fish ;\
	pushd $(abspath $(CACHEDIR)/fish) ;\
	F=`basename $(FISH_URL)` ;\
	! test -f $$F && curl -L -O $(FISH_URL) ;\
	! test -f $$F && exit 1 ;\
	D=`tar tf $$F | head -n 1` ;\
	tar xf $$F ;\
	pushd $$D ;\
	mkdir -p build ; cd build ;\
	cmake -DCMAKE_INSTALL_PREFIX=~/apps/fish .. && \
	make -j 4 && \
	make install

.ONESHELL: install-emacs
.PHONY: install-emacs
install-emacs:  ## download, compile and install emacs
	mkdir -p $(CACHEDIR)/emacs ;\
	pushd $(abspath $(CACHEDIR)/emacs) ;\
	F=`basename $(EMACS_URL)` ;\
	! test -f $$F && curl -L -O $(EMACS_URL) ;\
	! test -f $$F && exit 1 ;\
	D=`tar tf $$F | head -n 1` ;\
	! test -d $$D && tar xf $$F ;\
	pushd $$D ;\
	./configure --prefix=$$HOME/apps/emacs && \
	make -j 4 && \
	make install
