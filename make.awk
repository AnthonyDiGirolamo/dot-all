#!/usr/bin/gawk -E
@include "awklib/makefile"
@include "awklib/path"

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

function install_emacs() {
    download("emacs",
             "http://ftpmirror.gnu.org/emacs/emacs-27.2.tar.xz",
             "4c3d9ff35b2ab2fe518dc7eb3951e128")
}

function download(tool_name, url, expected_md5hash, cachedir,
                  _tempdir, _downloaded_file, _result) {
    if (!cachedir)
        cachedir = ".cache"
    _tempdir = cachedir "/" tool_name

    path::mkdirp(_tempdir)
    path::pushd(_tempdir)

    _downloaded_file = path::basename(url)
    if (!path::is_file(_downloaded_file)) {
        print cli::green("  [GET] ") url
        # TODO run curl -L -O
    }
    print cli::green("  [DONE] ") url
    _result = path::md5sum(_downloaded_file)
    if (_result != expected_md5hash "  " _downloaded_file) {
        print cli::error("  [ERROR] ") "MD5 mismatch"
        print cli::error("  [ERROR] ") "Expected: " expected_md5hash "  " _downloaded_file
        print cli::error("  [ERROR] ") "     Got: " _result
    }

    path::popd()
}
