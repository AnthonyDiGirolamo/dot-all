#!/usr/bin/gawk
@include "lib/getopt"
@include "lib/cli"

@namespace "make"

BEGIN {
    awk::Opterr = 1
    awk::Optind = 1

    cli::LOG_DEBUG = 0
    awk::DRYRUN = 0
    Help[""] = ""

    # Traverse array ordered by indices in ascending order compared as strings
    PROCINFO["sorted_in"] = "@ind_str_asc"

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
            awk::DRYRUN = 1
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

    if (targets_run == 0)
        usage()

    exit 0
}

function _run_target(target_name) {
    # Indirect call args as functions
    the_function = "make_targets::" target_name
    print( cli::green("[TARGET] ")  target_name)
    _result = @the_function()
}

function help(target_name, target_help) {
    MakeHelp[target_name] = target_help
}

function print_target_help(i) {
    # for(i in SYMTAB) {
    #     # if (match(i, /^make_targets::help_(.*)$/, mg)) {
    #     if (match(i, /^make::(.*)$/, mg)) {
    #         print(i, mg[1])
    #         if (awk::isarray(SYMTAB[i])) {
    #             cli::print_debug_array(SYMTAB[i])
    #         }
    #         else {
    #             print mg[1], "\t", SYMTAB[i]
    #         }
    #     }
    # }

    print "Target Help"
    for (i in Help) {
        print i, "\t", Help[i]
    }
}

function usage() {
    print "usage: make.awk [-v] [-d] TARGET ..."
    print "options:"
    print "  -v, --verbose     show debug output"
    print "  -d, --dry-run     don't write any files"

    print_target_help()
    # cli::print_debug_array(FUNCTAB)
    exit 1
}

function run(_command) {
    cli::print_debug("[RUN] " _command)
    system(_command)
}
