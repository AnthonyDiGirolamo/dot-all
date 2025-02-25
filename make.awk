#!/usr/bin/env -S AWKPATH=./awkpath gawk -E
@include "awkpath/makefile"
@include "awkpath/path"

@namespace "make_targets"

function help() {
    make::print_help("t,                  tangle",   "tangle all org files")
    make::print_help("build",             "run test & lint")
    make::print_help("test",              "awk unit tests")
    make::print_help("lint",              "awk linting")
    make::print_help("install_emacs28",   "download, compile and install emacs28")
    make::print_help("install_emacs29",   "download, compile and install emacs29")
    make::print_help("install_emacs30",   "download, compile and install emacs30")
    make::print_help("install_emacs_git", "download, compile and install emacs from git")
    make::print_help("install_fish",      "download, compile and install fish")
    make::print_help("install_fish_git",  "download, compile and install fish from git")
    make::print_help("install_lua54",     "download, compile and install lua 5.4")
}

function build() {
    test()
    lint()
}

function test() {
    path::pushd("awkpath")
    setawkpath ="env AWKPATH=. "
    make::run(setawkpath "gawk -f ./cli_test.awk")
    make::run(setawkpath "gawk -f ./tangle_test.awk")
    make::run(setawkpath "gawk -f ./path_test.awk")
    path::popd()
}

function lint() {
    make::run("env TANGLEAWK_DRYRUN=1 " \
              "gawk --lint=no-ext -f ./awkpath/tangle.awk *.org " \
              "1>/dev/null")
}

function t() {
    tangle()
}

function tangle() {
    make::run("./awkpath/tangle.awk *.org")
}

function install_emacs_git() {
    # Install emacs from Git:
    clonedir = make::git_clone("emacs",
        "https://git.savannah.gnu.org/git/emacs.git",
        "master")  # Branch
    if (!path::is_file(clonedir "/configure")) {
        path::pushd(clonedir)
        make::run("./autogen.sh")
        path::popd()
    }
    make::compile(clonedir,
        "./configure --prefix=$HOME/apps/emacs_git " \
        "--with-modules --with-cairo " \
        "--with-native-compilation " \
        "--with-x-toolkit=gtk3 --without-xaw3d\n" \
        "make -j 4\n" \
        "make install\n")
}

function install_emacs28() {
    tarfile = make::download("emacs",
        "http://ftpmirror.gnu.org/emacs/emacs-28.2.tar.xz",
        "cb799cdfc3092272ff6d35223fc6bfef")
    make::compile(make::extract_tar(tarfile),
        "./configure --prefix=$HOME/apps/emacs28 " \
        "--with-modules --with-cairo " \
        "--with-native-compilation " \
        "--with-x-toolkit=gtk3 --without-xaw3d\n" \
        "make -j 4\n" \
        "make install\n")
}

function install_treesitter() {
    clonedir = make::git_clone("tree-sitter",
        "https://github.com/tree-sitter/tree-sitter",
        "v0.20.8")  # Branch
    make::compile(clonedir,
        "make -j 4\n" \
        "make PREFIX=$HOME/apps/tree-sitter install\n")
}

function install_emacs29() {
    # Tree-sitter environment flags for configure:
    # "env " \
    # "TREE_SITTER_CFLAGS=\"-I${HOME}/apps/tree-sitter/include\" " \
    # "TREE_SITTER_LIBS=\"-L${HOME}/apps/tree-sitter/lib -ltree-sitter\" " \

    # Tree-sitter environment flags for make && make install:
    # "env " \
    # "LD_LIBRARY_PATH=\"${HOME}/apps/tree-sitter/lib\" " \

    # Assume tree-sitter is installed with apt/pacman:
    # "--with-tree-sitter "

    tarfile = make::download("emacs",
        "https://ftpmirror.gnu.org/emacs/emacs-29.4.tar.xz",
        "b9cc42f7d8066152535cf267418b8ced")

    make::compile(make::extract_tar(tarfile),
        "./configure --prefix=$HOME/apps/emacs29 " \
        "--with-modules " \
        "--with-cairo " \
        "--with-native-compilation " \
        "--with-pgtk " \
        "--without-xaw3d\n" \
        "make -j 8\n" \
        "make install\n")
}

function install_emacs30() {
    tarfile = make::download("emacs",
        "https://ftpmirror.gnu.org/emacs/emacs-30.1.tar.xz",
        "88d665794fdecba7769c11ab1ccea6e4")

    make::compile(make::extract_tar(tarfile),
        "./configure --prefix=$HOME/apps/emacs30 " \
        "--with-modules " \
        "--with-cairo " \
        "--with-native-compilation " \
        "--with-pgtk " \
        "--without-xaw3d\n" \
        "make -j 8\n" \
        "make install\n")
}

function install_lua54() {
    tarfile = make::download("lua54",
        "https://www.lua.org/ftp/lua-5.4.4.tar.gz",
        "bd8ce7069ff99a400efd14cf339a727b")
    make::compile(make::extract_tar(tarfile),
        "make linux -j 4\n" \
        "make CC=clang INSTALL_TOP=$HOME/apps/lua54 install\n")
}

function install_fish() {
    tarfile = make::download("fish",
        "https://github.com/fish-shell/fish-shell/releases/download/3.7.0/fish-3.7.0.tar.xz",
        "22c3fab479b185faf620a3b3f43443c3")
    make::compile(make::extract_tar(tarfile),
        "cmake -S . -B build " \
        "-DCMAKE_INSTALL_PREFIX=~/apps/fish\n" \
        "cmake --build build\n" \
        "cmake --install build\n")
}

function install_fish_git() {
    clonedir = make::git_clone("fish-shell",
        "https://github.com/fish-shell/fish-shell",
        "master")  # Branch
    make::compile(clonedir,
        "cmake -S . -B build " \
        "-DCMAKE_INSTALL_PREFIX=~/apps/fish\n" \
        "cmake --build build\n" \
        "cmake --install build\n")
}
