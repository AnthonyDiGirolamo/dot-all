#!/usr/bin/gawk -E
@include "lib/make"
@namespace "make_targets"

make::Help["build"] = "run test & lint"
function build() {
    test()
    lint()
}

make::help("test", "run awk unit tests")
function test() {
    make::run("gawk -f ./tangle_test.awk")
}

make::help("lint", "run linting on tangle.awk")
function lint() {
    make::run("env TANGLEAWK_DRYRUN=1 " \
              "gawk --lint=no-ext -f ./tangle.awk *.org " \
              "1>/dev/null")
}

make::help("t", "run tangle.awk on all org files")
function t() {
    make::run("./tangle.awk *.org")
}

