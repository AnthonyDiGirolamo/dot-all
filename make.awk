#!/usr/bin/gawk -E
@include "bin/getopt"

BEGIN {
    Opterr = 1
    Optind = 1

    LOG_DEBUG = 0
    DRYRUN = 0

    uname_system_type = _get_uname_system_type()
    hostname = _get_hostname()

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

    _DEBUG("Non-option arguments:")
    for (; Optind < ARGC; Optind++) {
        _DEBUG(sprintf("\tARGV[%d] = <%s>", Optind, ARGV[Optind]))
        # Indirect call args as functions
        the_function = ARGV[Optind]
        _result = @the_function()
    }
}

function usage() {
    print "usage: make.awk [-v] [-d] TARGET ..."
    print "options:"
    print "  -v, --verbose     show debug output"
    print "  -d, --dry-run     don't write any files"
    exit 1
}

# Functions from tangle.awk
# TODO: dedup these?

function wrap_cli_color(colorcode, text) {
    if (ENVIRON["TERM"] == "dumb")
        return text
    else
        return sprintf("\033[%dm%s\033[0m", colorcode, text)
}

function cli_green(text) { return wrap_cli_color(32, text) }
function cli_warning(text) { return wrap_cli_color(33, text) }
function cli_error(text) { return wrap_cli_color(31, text) }
function cli_debug(text) { return wrap_cli_color(35, text) }

function _DEBUG(text) {
    if (LOG_DEBUG)
        print cli_debug("[DEBUG] ") text
}

function _DEBUG_ARRAY(a) {
    for (i in a)
        _DEBUG("[" i "]: '" a[i] "'")
}

function _get_uname_system_type(_system_type) {
    while (("uname -a" |& getline line) > 0) {
        uname = line
    }
    close("uname -a")
    _DEBUG("uname = " uname)
    _system_type = "gnu/linux"
    if (uname ~ uname_msys_regex)
        _system_type = "windows-nt"
    else if (uname ~ uname_macos_regex)
        _system_type = "darwin"
    _system_type = trim_whitespace(_system_type)
    _DEBUG("uname_system_type = " _system_type)
    return _system_type
}

function _get_hostname(_hostname) {
    while (("hostname" |& getline line) > 0) {
        _hostname = line
    }
    close("hostname")
    _hostname = trim_whitespace(_hostname)
    _DEBUG("hostname = " _hostname)
    return _hostname
}

function trim_whitespace(text,
                         _trimmed_text) {
    trimmed_text = text
    sub(/^\s+/, "", trimmed_text)
    sub(/\s+$/, "", trimmed_text)
    return trimmed_text
}
