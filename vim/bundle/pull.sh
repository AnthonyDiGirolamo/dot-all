#!/bin/bash
urls="
https://github.com/NLKNguyen/papercolor-theme
https://github.com/christoomey/vim-tmux-navigator.git
https://github.com/ctrlpvim/ctrlp.vim.git
https://github.com/gerardbm/vim-atomic.git
https://github.com/itchyny/lightline.vim
https://github.com/jceb/vim-orgmode.git
https://github.com/junegunn/fzf.vim
https://github.com/keremc/asyncomplete-clang.vim
https://github.com/mattn/vim-lsp-settings.git
https://github.com/prabirshrestha/async.vim
https://github.com/prabirshrestha/asyncomplete.vim.git
https://github.com/prabirshrestha/vim-lsp.git
https://github.com/rakr/vim-one.git
https://github.com/tpope/vim-commentary.git
https://github.com/tpope/vim-dispatch.git
https://github.com/tpope/vim-flagship.git
https://github.com/tpope/vim-fugitive.git
https://github.com/tpope/vim-repeat.git
https://github.com/tpope/vim-sensible.git
https://github.com/tpope/vim-sleuth.git
https://github.com/tpope/vim-speeddating.git
https://github.com/tpope/vim-surround.git
https://github.com/tpope/vim-tbone.git
https://github.com/tpope/vim-unimpaired.git
https://github.com/tpope/vim-vinegar.git
https://github.com/tpope/vim-vividchalk.git
https://github.com/vim-airline/vim-airline-themes.git
https://github.com/vim-airline/vim-airline.git
https://github.com/vim-syntastic/syntastic.git
https://github.com/ycm-core/YouCompleteMe.git
"
urls="
"
for URL in $urls
do
       GDIR=$(basename -s .git $URL)
       echo $URL
       echo "-> ${GDIR}"
       rm -rf $GDIR
       git clone --depth=1 --branch=master --recurse-submodules $URL
       rm -rf $GDIR/.git
       git add -A $GDIR
done
