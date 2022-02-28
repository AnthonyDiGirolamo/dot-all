#!/usr/bin/gawk -E
@include "lib/getopt"
@include "lib/printing"

BEGIN {
    Opterr = 1
    Optind = 1

    LOG_DEBUG = 0
    DRYRUN = 0

    _myshortopts = "vdh"
    _mylongopts = "verbose,dry-run,help"

    while ((_option = getopt(ARGC, ARGV, _myshortopts, _mylongopts)) != -1) {
        switch (_option) {
        case "v":
        case "verbose":
            LOG_DEBUG = 1
            break
        case "d":
        case "dry-run":
            DRYRUN = 1
            break
        case "?":
        case "h":
        case "help":
        # default:
            usage()
            break
        }
        _DEBUG(sprintf("Handle option: %s, Optarg = '%s'", _option, Optarg))
    }
    # Erase handled args
    for (i = 1; i < Optind; i++)
        ARGV[i] = ""

    uname_system_type = _get_uname_system_type()
    hostname = _get_hostname()

    targets_run = 0
    _DEBUG("Non-option arguments:")
    for (; Optind < ARGC; Optind++) {
        _DEBUG(sprintf("\tARGV[%d] = <%s>", Optind, ARGV[Optind]))
        # Indirect call args as functions
        the_function = "target_" ARGV[Optind]

        print( cli_green("[TARGET] ") ARGV[Optind] )

        _result = @the_function()
        targets_run += 1
    }

    if (targets_run == 0)
        usage()
}

function usage() {
    print "usage: make.awk [-v] [-d] TARGET ..."
    print "options:"
    print "  -v, --verbose     show debug output"
    print "  -d, --dry-run     don't write any files"
    exit 1
}

# TARGET functions

function target_all() {
    target_test()
    target_lint()
}

function target_test() {
    run_command("gawk -f ./tangle_test.awk")
}

function target_lint() {
    run_command("env TANGLEAWK_DRYRUN=1 gawk --lint=no-ext -f ./tangle.awk *.org 1>/dev/null")
}

function target_t() {
    run_command("./tangle.awk *.org")
}

function run_command(_command) {
    _DEBUG("[RUN] " _command)
    system(_command)
}

