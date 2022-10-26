#!/usr/bin/env -S AWKPATH=./awkpath gawk -E
@include "awkpath/makefile"
@include "awkpath/path"

@namespace "make_targets"

function help() {
    make::print_help("t",             "tangle all org files")
    make::print_help("build",         "run test & lint")
    make::print_help("test",          "awk unit tests")
    make::print_help("lint",          "awk linting")
    make::print_help("install_emacs", "download, compile and install emacs27")
    make::print_help("install_emacs28", "download, compile and install emacs28")
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
    make::run("./awkpath/tangle.awk *.org")
}

function install_emacs() {
    tarfile = make::download("emacs",
        "http://ftpmirror.gnu.org/emacs/emacs-27.2.tar.xz",
        "4c3d9ff35b2ab2fe518dc7eb3951e128")
    make::compile(make::extract_tar(tarfile),
        "./configure --prefix=$HOME/apps/emacs --with-modules --with-cairo\n" \
        "make -j 4\n" \
        "make install\n")
}

function install_emacs28() {
    # From Git:
    # clonedir = make::git_clone("emacs28",
    #     "https://git.savannah.gnu.org/git/emacs.git",
    #     # Branch
    #     "emacs-28")
    # if (!path::is_file(clonedir "/configure")) {
    #     path::pushd(clonedir)
    #     make::run("./autogen.sh")
    #     path::popd()
    # }
    # make::compile(clonedir, ...)

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

function install_lua54() {
    tarfile = make::download("lua54",
        "https://www.lua.org/ftp/lua-5.4.0.tar.gz",
        "dbf155764e5d433fc55ae80ea7060b60")
    make::compile(make::extract_tar(tarfile),
        "make linux -j 4\n" \
        "make INSTALL_TOP=$HOME/apps/lua54 install\n")
}

function install_fish() {
    tarfile = make::download("fish",
        "https://github.com/fish-shell/fish-shell/releases/download/3.3.1/fish-3.3.1.tar.xz",
        "94be285255aadfcf0f910bdcc2f56073")
    make::compile(make::extract_tar(tarfile),
        "cmake -DCMAKE_INSTALL_PREFIX=~/apps/fish .\n" \
        "make -j 4\n" \
        "make install\n")
}
