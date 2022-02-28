#!/usr/bin/gawk
@include "awklib/cli"
@include "awklib/getopt"

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
        # cli::print_debug_array(FUNCTAB)
        # usage()
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

function run(_command) {
    cli::print_debug("[RUN] " _command)
    if (!make::DRYRUN)
        system(_command)
}
