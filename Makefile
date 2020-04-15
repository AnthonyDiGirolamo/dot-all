SHELL=bash

# First rule (help) is the default if make is run with no targets
.PHONY: help
help:  ## show help
	@grep -hE '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
# Help grep source: https://marmelab.com/blog/2016/02/29/auto-documented-makefile.html

CACHEDIR := ./.cache

FISH_URL := https://github.com/fish-shell/fish-shell/releases/download/3.1.0/fish-3.1.0.tar.gz
EMACS_URL := http://ftpmirror.gnu.org/emacs/emacs-26.3.tar.xz

ORG_FILES := $(wildcard *.org)
ORG_OUT_FILES := $(foreach f, $(ORG_FILES),$(CACHEDIR)/$(basename $(f)).out)

.PHONY: clean
clean:  ## delete .cache
	rm -rf $(CACHEDIR)

.PHONY: mkdirs
mkdirs:
	@mkdir -p $(CACHEDIR)

all: clean tangle symlinks ## clean and tangle all
tangle: mkdirs $(ORG_OUT_FILES)  ## tangle all dotfiles

.PHONY: symlinks
.ONESHELL:
symlinks:  ## symlink folders
	@ln -snvf $(abspath ./vim) $(HOME)/.vim
	@ln -snvf $(abspath ./emacs.d) $(HOME)/.emacs.d

# @ln -snvf $(abspath ./emacs.d) $(HOME)/.emacs.d

.PHONY: install-fish
.ONESHELL:
install-fish:  ## download, compile and install fish
	@mkdir -p $(CACHEDIR)/fish
	cd $(abspath $(CACHEDIR)/fish)
	F=`basename $(FISH_URL)`
	! test -f $$F && echo [CURL] $(FISH_URL) && curl -L -O $(FISH_URL) 2>/dev/null
	! test -f $$F && exit 1
	echo [DOWNLOADED] $$F
	D=`tar tf $$F | head -n 1`
	tar xf $$F
	cd $$D
	mkdir -p build ; cd build
	cmake -DCMAKE_INSTALL_PREFIX=~/apps/fish ..
	make -j 4
	make install
	cd $(abspath $(CACHEDIR)/fish)
	rm -rf $$D

.PHONY: install-emacs
.ONESHELL:
install-emacs:  ## download, compile and install emacs
	@mkdir -p $(CACHEDIR)/emacs
	cd $(abspath $(CACHEDIR)/emacs)
	F=`basename $(EMACS_URL)`
	! test -f $$F && echo [CURL] $(EMACS_URL) && curl -L -O $(EMACS_URL) 2>/dev/null
	! test -f $$F && exit 1
	echo [DOWNLOADED] $$F
	D=`tar tf $$F | head -n 1`
	! test -d $$D && tar xf $$F
	cd $$D
	./configure --prefix=$$HOME/apps/emacs
	make -j 4
	make install
	cd $(abspath $(CACHEDIR)/emacs)
	rm -rf $$D

# Rule to convert a *.org file to a .cache/*.out
$(CACHEDIR)/%.out: %.org
	@echo "[TANGLE] $<"
	@test -f $@ && cp $@ $@.last
	@rm -f $@
	@emacs -Q --batch --eval "(progn \
(require 'org) \
(require 'ob-shell) \
(setq make-backup-files nil) \
(setq org-confirm-babel-evaluate nil) \
(defun amd/post-tangle () \
(let ((tangled-output-file (buffer-file-name)) \
(dot-out-file (car command-line-args-left))) \
(save-excursion \
(find-file dot-out-file) \
(goto-char (point-max)) \
(insert (format \"%s\n\" tangled-output-file)) \
(sort-lines nil (point-min) (point-max)) \
(save-buffer)) \
(princ (format \"    %s\n\" tangled-output-file) t)) \
) \
(add-hook 'org-babel-post-tangle-hook 'amd/post-tangle) \
(org-babel-tangle-file \"$<\"))" $(abspath $@) 2>/dev/null
	@touch $@

# rule to make a directory
%/:
	@mkdir -p $@

# prevent intermediate files CACHEDIR and %.out from being deleted
.PRECIOUS: $(CACHEDIR)/ $(CACHEDIR)/%.out
