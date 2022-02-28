#!/usr/bin/gawk -E
@include "awklib/makefile"
@include "awklib/path"

@namespace "make_targets"

function help() {
    make::print_help("t",             "run tangle.awk on all org files")
    make::print_help("build",         "run test & lint")
    make::print_help("test",          "run awk unit tests")
    make::print_help("lint",          "run linting on tangle.awk")
    make::print_help("install_emacs", "download, compile and install emacs")
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
    compile(download("emacs",
                     "http://ftpmirror.gnu.org/emacs/emacs-27.2.tar.xz",
                     "4c3d9ff35b2ab2fe518dc7eb3951e128"),
            "ls\npwd")
            # "./configure --prefix=$HOME/apps/emacs --with-modules --with-cairo\n" \
            # "make -j 4\n"
            # "make install\n")
}

function compile(source_archive, commands,
                 _tmp_dir, _build_dir, _tar_command,
                 _index, _this_command, _command_list, _result) {
    _tmp_dir = path::dirname(source_archive)
    path::pushd(_tmp_dir)

    # Find top level folder used in the source_archive
    _tar_command = "tar tf " path::basename(source_archive) " | head -n1"
    _tar_command | getline _build_dir
    close(_tar_command)

    # Extract source_archive if needed
    if (!path::is_dir(_build_dir)) {
        print cli::green("  [Extract] ") source_archive
        _tar_command = "tar xf " path::basename(source_archive)
        make::run(_tar_command)
    }

    # Cd to extracted source folder
    path::pushd(_build_dir)

    split(commands, _command_list, "\n")
    for (_index in _command_list) {
        if (_command_list[_index] != "") {
            print cli::blue("==> ") _command_list[_index]
            _result = make::run(_command_list[_index])
            if (!_result)
                exit _result
        }
    }

    path::popd()
    make::run("rm -rf " _build_dir)
    path::popd()
}

function download(tool_name, url, expected_md5hash, cachedir,
                  _tempdir, _downloaded_file, _result) {
    # Create download folder
    if (!cachedir)
        cachedir = ".cache"
    _tempdir = cachedir "/" tool_name
    # Mkdir and cd
    path::mkdirp(_tempdir)
    path::pushd(_tempdir)

    # Download the file
    _downloaded_file = path::basename(url)
    if (!path::is_file(_downloaded_file)) {
        print cli::green("  [GET] ") url
        make::run("curl -L -O " url)
    }
    print cli::green("  [DONE] ") url

    # Run md5 checksum
    _result = path::md5sum(_downloaded_file)

    # Checksum matches?
    if (_result != expected_md5hash "  " _downloaded_file) {
        print cli::error("  [ERROR] ") "MD5 mismatch"
        print cli::error("  [ERROR] ") "Expected: " expected_md5hash "  " _downloaded_file
        print cli::error("  [ERROR] ") "     Got: " _result
    }

    # Restore cwd and return downloaded file
    return path::popd() "/" _downloaded_file
}
