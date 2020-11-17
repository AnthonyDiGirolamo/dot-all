SHELL := bash
# .ONESHELL:
# .SHELLFLAGS := -eu -o pipefail -c
# .DELETE_ON_ERROR:
# MAKEFLAGS += --warn-undefined-variables
# MAKEFLAGS += --no-builtin-rules
# ifeq ($(origin .RECIPEPREFIX), undefined)
#   $(error This Make does not support .RECIPEPREFIX. Please use GNU Make 4.0 or later)
# endif
# .RECIPEPREFIX = >

HOSTNAME := $(shell uname -n)

CACHEDIR := ./.cache

# First rule (help) is the default if make is run with no targets
.PHONY: help
help:  ## show help
	@grep -hE '^[0-9a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'
# Help grep source: https://marmelab.com/blog/2016/02/29/auto-documented-makefile.html

fish_url := https://github.com/fish-shell/fish-shell/releases/download/3.1.0/fish-3.1.0.tar.gz
fish_md5 := 8c9995a5a6d07ce05a1413ca24e16691

# emacs_url := http://ftpmirror.gnu.org/emacs/emacs-26.3.tar.xz
# emacs_md5 := 0a2e4b965d31a7cb1930eae3b79df793

emacs_url := http://ftpmirror.gnu.org/emacs/emacs-27.1.tar.xz
emacs_md5 := f1f25356b68ec69caccd41d95988d23d

lua54_url := https://www.lua.org/ftp/lua-5.4.0.tar.gz
lua54_md5 := dbf155764e5d433fc55ae80ea7060b60

lua_url := https://www.lua.org/ftp/lua-5.3.5.tar.gz
lua_md5 := 4f4b4f323fd3514a68e0ab3da8ce3455

luarocks_url := https://luarocks.org/releases/luarocks-3.3.1.tar.gz
luarocks_md5 := 1dc12df0b4dc312625a0d36b194b76ef

ORG_FILES := $(wildcard *.org)
ORG_OUT_FILES := $(foreach f, $(ORG_FILES),$(CACHEDIR)/$(basename $(f)).out)

.PHONY: clean
clean:  ## delete .cache
	rm -rf $(CACHEDIR)

all: clean t symlinks  ## clean and tangle all
t: tangle  ## tangle alias
tangle: tangleawk  ## tangleawk alias
tangleorg: mkdirs $(ORG_OUT_FILES) rm-removed-files  ## tangle all dotfiles with emacs+org-mode
tangleawk: mkdirs ## tangle all dotfiles with gawk
	@./tangle.awk *.org

tmux: mkdirs .cache/terminfo.out .cache/tmux.out

.PHONY: symlinks
.ONESHELL:
symlinks:  ## symlink folders
	@echo '[SYMLINKS]'
	@echo -n '  '
	@ln -snvf $(abspath ./vim) $(HOME)/.vim
	@echo -n '  '
	@ln -snvf $(abspath ./emacs.d) $(HOME)/.emacs.d

.PHONY: install-gtk-themes
.ONESHELL:
install-gtk-themes:  ## download and install gtk themes/icons
	@mkdir -p $(CACHEDIR)/themes
	@cd $(abspath $(CACHEDIR)/themes)
	git clone --depth=1 https://github.com/vinceliuice/Qogir-theme.git
	git clone --depth=1 https://github.com/vinceliuice/Tela-icon-theme.git
	cd Qogir-theme
	./install.sh --dest $(HOME)/.themes
	cd ../Tela-icon-theme
	./install.sh -d $(HOME)/.local/share/icons standard black blue brown green grey orange pink purple red yellow manjaro ubuntu

CD_TO_BUILD_DIR=cd $(abspath $(CACHEDIR)/$(subst build-,,$@)) ; URL="$($(subst build-,,$@)_url)" ; F=`basename $$URL` ; D=`tar tf $$F | head -n 1` ; ! test -d $$D && tar xf $$F ; cd $$D

.PHONY: build-fish
.ONESHELL:
build-fish: download-fish
	@echo "[BUILD] fish"
	$(CD_TO_BUILD_DIR)
	mkdir -p build ; cd build
	cmake -DCMAKE_INSTALL_PREFIX=~/apps/fish ..
	make -j 4
	make install
	cd $(abspath $(CACHEDIR)/fish)
	rm -rf $$D

.PHONY: build-emacs
.ONESHELL:
build-emacs: download-emacs
	@echo "[BUILD] emacs"
	$(CD_TO_BUILD_DIR)
	./configure --prefix=$$HOME/apps/emacs --with-modules --with-cairo
	make -j 4
	make install
	cd $(abspath $(CACHEDIR)/emacs)
	rm -rf $$D

.PHONY: build-lua54
.ONESHELL:
build-lua54: download-lua54
	@echo "[BUILD] lua"
	$(CD_TO_BUILD_DIR)
	make linux -j 4
	make INSTALL_TOP=$(abspath $(HOME)/apps/lua54) install
	cd $(abspath $(CACHEDIR)/lua54)
	rm -rf $$D

.PHONY: build-lua
.ONESHELL:
build-lua: download-lua
	@echo "[BUILD] lua"
	$(CD_TO_BUILD_DIR)
	make linux -j 4
	make INSTALL_TOP=$(abspath $(HOME)/apps/lua) install
	cd $(abspath $(CACHEDIR)/lua)
	rm -rf $$D

.PHONY: build-luarocks
.ONESHELL:
build-luarocks: download-luarocks
	@echo "[BUILD] luarocks"
	$(CD_TO_BUILD_DIR)
	./configure --prefix=$$HOME/apps/lua --with-lua=$$HOME/apps/lua
	make
	make install
	cd $(abspath $(CACHEDIR)/luarocks)
	rm -rf $$D

.PHONY: download-%
.ONESHELL:
download-%:
	@echo "[DOWNLOAD] $*"
	mkdir -p $(CACHEDIR)/$*
	cd $(abspath $(CACHEDIR)/$*)
	URL="$($*_url)"
	EXPECTED_MD5="$($*_md5)"
	F=`basename $$URL`
	! test -f $$F && echo [CURL] $$URL && curl -L -O $$URL 2>/dev/null
	! test -f $$F && exit 1
	md5hash=`md5sum $$F`
	echo [DOWNLOADED] $$md5hash
	test "$$EXPECTED_MD5  $$F" != "$$md5hash" && echo "Invalid MD5" && echo "   Expected: $$EXPECTED_MD5" && exit 1
	exit 0

.PHONY: lua54
.ONESHELL:
install-lua54: build-lua54 ## download, compile and install lua 5.4

.PHONY: lua
.ONESHELL:
install-lua: build-lua ## download, compile and install lua

.PHONY: luarocks
.ONESHELL:
install-luarocks: build-luarocks ## download, compile and install luarocks

.PHONY: emacs
.ONESHELL:
install-emacs: build-emacs ## download, compile and install emacs

.PHONY: fish
.ONESHELL:
install-fish: build-fish ## download, compile and install fish

xcape_git_url := https://github.com/alols/xcape.git
lux_git_url := https://github.com/Ventto/lux.git
clac_git_url := https://github.com/soveran/clac.git
base16shell_git_url := https://github.com/chriskempson/base16-shell.git

.PHONY: git-pull-%
.ONESHELL:
git-pull-%:
	@URL=$($(subst git-pull-,,$@)_git_url)
	GDIR=$$(basename -s .git $$URL)
	pushd source
	echo $$URL
	echo "-> $${GDIR}"
	rm -rf $$GDIR
	git clone --depth=1 --branch=master $$URL
	rm -rf $$GDIR/.git
	git add -A $$GDIR
	popd

.PHONY: update-xcape
.ONESHELL:
update-xcape: git-pull-xcape  ## git pull xcape source

.PHONY: update-lux
.ONESHELL:
update-lux: git-pull-lux  ## git pull lux source

.PHONY: update-clac
.ONESHELL:
update-clac: git-pull-clac  ## git pull clac source

.PHONY: update-base16shell
.ONESHELL:
update-base16shell: git-pull-base16shell  ## git pull base16-shell source

.PHONY: pip
.ONESHELL:
install-pip:  ## install python3 pip
	@mkdir -p $(CACHEDIR)/pip
	cd $(abspath $(CACHEDIR)/pip)
	curl https://bootstrap.pypa.io/get-pip.py -o get-pip.py
	python3 get-pip.py

.PHONY: rm-removed-files
.ONESHELL:
rm-removed-files:  ## rm files removed since last make tangle
	@for f in $(ORG_OUT_FILES); do
		! test -f $$f.last && continue
		cat $$f.last | sort > $$f.1
		cat $$f | sort > $$f.2
		for removed in $$(diff -u $$f.1 $$f.2 | grep -E '^-[^-].*' | sed -e 's/^-\(.*\)/\1/'); do
			test -f $$removed && rm -i $$removed
		done
		rm $$f.1 $$f.2
	done
	exit 0

.PHONY: rm-all-tangled-files
.ONESHELL:
rm-all-tangled-files:  ## rm all tangled files
	@for outfile in $(ORG_OUT_FILES); do
		for f in $$(test -f $$outfile && cat $$outfile); do
			test -f $$f && rm -v $$f
		done
		rm -f $$outfile $$outfile.last
	done
	exit 0

# Rule to convert a *.org file to a .cache/*.out
$(CACHEDIR)/%.out: %.org
	@echo "[TANGLE] $<"
	@test -f $@ && cp $@ $@.last
	@rm -f $@
	@emacs -Q --batch --eval "(progn \
	(require 'org) \
	(require 'ob-shell) \
	(setq make-backup-files nil) \
	(defalias 'yes-or-no-p 'y-or-n-p) \
	(setq org-confirm-babel-evaluate nil) \
	(setq hostname \"$(HOSTNAME)\")
	(defun tangle-file-if (file p) (if (eval p) file \"no\"))
	(defun amd/post-tangle () \
	  (let ((tangled-output-file (buffer-file-name)) \
		(dot-out-file (car command-line-args-left))) \
	      (save-excursion \
		(find-file dot-out-file) \
		(goto-char (point-max)) \
		(insert (format \"%s\n\" tangled-output-file)) \
		(sort-lines nil (point-min) (point-max)) \
		(save-buffer)) \
	      (princ (format \"  %s\n\" tangled-output-file) t)) \
	) \
	(add-hook 'org-babel-post-tangle-hook 'amd/post-tangle) \
	(org-babel-tangle-file \"$<\") \
	(org-babel-map-src-blocks \"$<\" \
	  (when (string-match-p \":eval yes\" header-args)
	    (princ (format \"%s\" (org-babel-execute-src-block))))) \
	)" $(abspath $@) 2>/dev/null
	@touch $@


.PHONY: mkdirs
mkdirs: $(CACHEDIR)/

# rule to make a directory
%/:
	@mkdir -v -p $@

# prevent intermediate files CACHEDIR and %.out from being deleted
.PRECIOUS: $(CACHEDIR)/ $(CACHEDIR)/%.out
