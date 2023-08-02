#!/usr/bin/env -S AWKPATH=./awkpath gawk -E
@include "awkpath/makefile"
@include "awkpath/path"

@namespace "make_targets"

function help() {
    make::print_help("t, tangle",     "tangle all org files")
    make::print_help("build",         "run test & lint")
    make::print_help("test",          "awk unit tests")
    make::print_help("lint",          "awk linting")
    make::print_help("install_emacs28", "download, compile and install emacs28")
    make::print_help("install_emacs29", "download, compile and install emacs29")
    make::print_help("install_emacs_git", "download, compile and install emacs from git")
    make::print_help("install_fish",  "download, compile and install fish-shell")
    make::print_help("install_lua54", "download, compile and install lua 5.4")
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

function install_emacs27() {
    tarfile = make::download("emacs",
        "http://ftpmirror.gnu.org/emacs/emacs-27.2.tar.xz",
        "4c3d9ff35b2ab2fe518dc7eb3951e128")
    make::compile(make::extract_tar(tarfile),
        "./configure --prefix=$HOME/apps/emacs27 " \
        "--with-modules --with-cairo\n" \
        "make -j 2\n" \
        "make install\n")
}

function install_emacs_git() {
    # Install emacs from Git:
    clonedir = make::git_clone("emacs",
        "https://git.savannah.gnu.org/git/emacs.git",
        # Branch
        "master")
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
        "make -j 2\n" \
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
        "make -j 2\n" \
        "make install\n")
}

function install_emacs29() {
    tarfile = make::download("emacs",
        "https://ftpmirror.gnu.org/emacs/emacs-29.1.tar.xz",
        "e0631d868a13b503a5feef042435b67c")
    make::compile(make::extract_tar(tarfile),
        "./configure --prefix=$HOME/apps/emacs29 " \
        "--with-modules --with-cairo " \
        "--with-native-compilation " \
        "--with-x-toolkit=gtk3 --without-xaw3d\n" \
        "make -j 2\n" \
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
        "https://github.com/fish-shell/fish-shell/releases/download/3.6.1/fish-3.6.1.tar.xz",
        "b721bbff7adc221f4e77b44404727026")
    make::compile(make::extract_tar(tarfile),
        "cmake -DCMAKE_INSTALL_PREFIX=~/apps/fish .\n" \
        "make -j 4\n" \
        "make install\n")
}
