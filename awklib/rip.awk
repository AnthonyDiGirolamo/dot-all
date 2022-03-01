#!/usr/bin/gawk -E
@include "awklib/cli"
@include "awklib/getopt"
@include "awklib/path"
@include "awklib/with"

@namespace "ripawk"

BEGIN {
    awk::Opterr = 1
    awk::Optind = 1

    cli::LOG_DEBUG = 0

    _myshortopts = "vg:h"
    _mylongopts = "verbose,file-pattern:help"

    file_pattern = ""
    search_pattern = ""
    search_path = "."

    while ((_option = awk::getopt(ARGC, ARGV, _myshortopts, _mylongopts)) != -1) {
        switch (_option) {
        case "v":
        case "verbose":
            cli::LOG_DEBUG = 1
            break
        case "g":
        case "file-pattern":
            file_pattern = awk::Optarg
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

    # First arg is the search_pattern
    search_pattern = ARGV[awk::Optind]
    # Second arg if present is the search path
    if (awk::Optind + 1 in ARGV)
        search_path = ARGV[awk::Optind + 1]

    # Debug print
    # cli::print_debug("Non-option arguments:")
    # for (; awk::Optind < ARGC; awk::Optind++) {
    #     cli::print_debug(sprintf("\tARGV[%d] = <%s>",
    #                              awk::Optind,
    #                              ARGV[awk::Optind]))
    # }

    # Debug print
    cli::print_debug("  file_pattern = " file_pattern)
    cli::print_debug("search_pattern = " search_pattern)
    cli::print_debug("   search_path = " search_path)

    if (search_pattern == "") {
        print cli::error("Error: ") "Missing pattern"
        usage()
    }

    # Collect files matching the pattern
    path::glob(search_path, file_pattern)
    with::sort_index_string_asc("ripawk::search_all_files")

    exit 0
}

function search_all_files() {
    # Print filenames
    for (f in path::globdata_flattened) {
        search(path::globdata_flattened[f], search_pattern)
    }
}

function search(file_path, search_pattern,
                _linenumber, _line, _firstmatch_found) {
    _linenumber = 1
    _firstmatch_found = 0
    while ((getline _line < file_path) > 0) {
        if (_line ~ search_pattern) {
            # Print filename
            if (!_firstmatch_found) {
                print "\n" cli::magenta(file_path)
                _firstmatch_found = 1
            }
            # Print line number and matching line
            print cli::green(_linenumber) ":" _line
        }
        _linenumber++
    }
    close(file_path)
}

# readfile -- read an entire file into a variable
function readfile(file_path,
                  _text, _original_rs) {
    _original_rs = RS
    RS = "^$"
    getline _text < file_path
    close(file_path)
    RS = _original_rs
    return _text
}


function usage() {
    print "usage: rip.awk [-v] [-g] PATTERN PATH"
    print "options:"
    print "  -v, --verbose       show debug output"
    print "  -g, --file-pattern  Optional: regex pattern for files to search"
    exit 1
}
