#!/usr/bin/gawk
@include "awklib/cli"
@include "awklib/getopt"
@include "awklib/path"

@namespace "make"

BEGIN {
    awk::Opterr = 1
    awk::Optind = 1

    cli::LOG_DEBUG = 0
    make::DRYRUN = 0

    # Traverse array ordered by indices in ascending order compared as strings
    # PROCINFO["sorted_in"] = "@ind_str_asc"

    _myshortopts = "vdh"
    _mylongopts = "verbose,dry-run,help"

    while ((_option = awk::getopt(ARGC, ARGV, _myshortopts, _mylongopts)) != -1) {
        switch (_option) {
        case "v":
        case "verbose":
            cli::LOG_DEBUG = 1
            break
        case "d":
        case "dry-run":
            make::DRYRUN = 1
            break
        case "?":
        case "h":
        case "help":
        # default:
            usage()
            break
        }
        cli::print_debug(sprintf("Handle option: %s, Optarg = '%s'", _option, awk::Optarg))
    }
    # Erase handled args
    for (i = 1; i < awk::Optind; i++)
        ARGV[i] = ""

    uname_system_type = cli::get_uname_system_type()
    hostname = cli::get_hostname()

    targets_run = 0
    cli::print_debug("Non-option arguments:")
    for (; awk::Optind < ARGC; awk::Optind++) {
        cli::print_debug(sprintf("\tARGV[%d] = <%s>",
                                 awk::Optind,
                                 ARGV[awk::Optind]))

        _run_target(ARGV[awk::Optind])

        targets_run += 1
    }

    if (targets_run == 0) {
        if ("make_targets::help" in FUNCTAB)
            make_targets::help()
        else
            usage()
    }

    exit 0
}

function print_help(name, help_text,
                    max_length, target_length, format_string, i) {
    max_length = 0
    for (i in FUNCTAB) {
        if (match(i, /^make_targets::(.*)$/, mg)) {
            target_length = length(mg[1])
            if (target_length > max_length)
                max_length = target_length
        }
    }
    format_string = sprintf("%%-%ds %%s\n", max_length + 4 + length(cli::cyan("")))
    printf(format_string, cli::cyan(name), help_text)
}

function _run_target(target_name) {
    # Indirect call args as functions
    the_function = "make_targets::" target_name

    if (the_function in FUNCTAB) {
        print( cli::green("[TARGET] ")  target_name)
        _result = @the_function()
    }
    else {
        print cli::error("[ERROR]"), "Unknown target:", target_name
    }
}

function usage() {
    print "usage: make.awk [-v] [-d] TARGET ..."
    print "options:"
    print "  -v, --verbose     show debug output"
    print "  -d, --dry-run     don't run any system commands"
    exit 1
}

function run(command,
             _return_value) {
    _return_value = 0
    cli::print_debug("[RUN] " command)
    if (!make::DRYRUN) {
        _return_value = system(command)
        system("")  # flush output
    }
    return _return_value == 0
}

function compile(source_archive, commands,
                  # local vars
                 _tmp_dir, _build_dir, _tar_command,
                 _index, _this_command, _command_list, _result) {
    # cd to the dir containing the source_archive
    _tmp_dir = path::dirname(source_archive)
    path::pushd(_tmp_dir)

    # Find top level folder used in the source_archive
    _tar_command = "tar tf " path::basename(source_archive) " | head -n1"
    _tar_command | getline _build_dir
    close(_tar_command)

    # Extract source_archive if needed
    if (!path::is_dir(_build_dir)) {
        print cli::blue("==> ") cli::green("tar xf ") source_archive
        _tar_command = "tar xf " path::basename(source_archive)
        make::run(_tar_command)
    }

    # Cd to extracted source folder
    path::pushd(_build_dir)

    # Split the commands string by \n
    split(commands, _command_list, "\n")
    for (_index in _command_list) {
        if (_command_list[_index] != "") {
            print cli::blue("==> ") _command_list[_index]
            _result = make::run(_command_list[_index])
            # Run each command; exit if error
            if (!_result) {
                print cli::red("FAILED: ") _command_list[_index] > "/dev/stderr"
                exit 1
            }
        }
    }

    # cd to source_archive parent dir
    path::popd()
    # delete extracted source
    make::run("rm -rf " _build_dir)
    # cd to original dir
    path::popd()
}

function download(tool_name, url, expected_md5hash, cachedir,
                  # local vars
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
        print cli::green("[GET] ") url
        make::run("curl -L -O " url)
    }
    print cli::green("[DOWNLOADED] ") url

    # Run md5 checksum
    _result = path::md5sum(_downloaded_file)

    # Checksum matches?
    if (_result != expected_md5hash "  " _downloaded_file) {
        print cli::error("[ERROR] ") "MD5 mismatch"
        print cli::error("[ERROR] ") "Expected: " expected_md5hash "  " _downloaded_file
        print cli::error("[ERROR] ") "     Got: " _result
    }

    # Restore cwd and return downloaded file
    return path::popd() "/" _downloaded_file
}
