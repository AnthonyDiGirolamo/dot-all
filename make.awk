#!/usr/bin/gawk -E
@include "lib/make"
@namespace "make_targets"

function help() {
    make::print_help("build",
                     "run test & lint")
    make::print_help("test",
                     "run awk unit tests")
    make::print_help("lint",
                     "run linting on tangle.awk")
    make::print_help("t",
                     "run tangle.awk on all org files")
}

function build() {
    test()
    lint()
}

function test() {
    make::run("gawk -f ./tangle_test.awk")
}

function lint() {
    make::run("env TANGLEAWK_DRYRUN=1 " \
              "gawk --lint=no-ext -f ./tangle.awk *.org " \
              "1>/dev/null")
}

function t() {
    make::run("./tangle.awk *.org")
}
