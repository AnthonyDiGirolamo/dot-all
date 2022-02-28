#!/usr/bin/gawk -E
@include "awklib/makefile"
@include "awklib/path"

@namespace "make_targets"

function help() {
    make::print_help("t",             "tangle all org files")
    make::print_help("build",         "run test & lint")
    make::print_help("test",          "awk unit tests")
    make::print_help("lint",          "awk linting")
    make::print_help("install_emacs", "download, compile and install emacs")
    make::print_help("install_lua54", "download, compile and install lua 5.4")
}

function build() {
    test()
    lint()
}

function test() {
    path::pushd("awklib")
    make::run("gawk -f cli_test.awk")
    make::run("gawk -f tangle_test.awk")
    path::popd()
}

function lint() {
    make::run("env TANGLEAWK_DRYRUN=1 " \
              "gawk --lint=no-ext -f ./awklib/tangle.awk *.org " \
              "1>/dev/null")
}

function t() {
    make::run("./awklib/tangle.awk *.org")
}

function install_emacs() {
    tarfile = make::download("emacs",
        "http://ftpmirror.gnu.org/emacs/emacs-27.2.tar.xz",
        "4c3d9ff35b2ab2fe518dc7eb3951e128")
    make::compile(tarfile,
        "./configure --prefix=$HOME/apps/emacs --with-modules --with-cairo\n" \
        "make -j 4\n" \
        "make install\n")
}

function install_lua54() {
    tarfile = make::download("lua54",
        "https://www.lua.org/ftp/lua-5.4.0.tar.gz",
        "dbf155764e5d433fc55ae80ea7060b60")
    make::compile(tarfile,
        "make linux -j 4\n" \
        "make INSTALL_TOP=$HOME/apps/lua54 install\n")
}

