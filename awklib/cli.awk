#!/usr/bin/gawk -f
@namespace "cli"

function color(colorcode, text) {
    if (ENVIRON["TERM"] == "dumb")
        return text
    else
        return sprintf("\033[%dm%s\033[0m", colorcode, text)
}

function reset()       { return "\033[0m" }
function black(text)   { return color(30, text) }
function red(text)     { return color(31, text) }
function green(text)   { return color(32, text) }
function yellow(text)  { return color(33, text) }
function blue(text)    { return color(34, text) }
function magenta(text) { return color(35, text) }
function cyan(text)    { return color(36, text) }
function white(text)   { return color(37, text) }

# Bright Black:   30;1
# Bright Red:     31;1
# Bright Green:   32;1
# Bright Yellow:  33;1
# Bright Blue:    34;1
# Bright Magenta: 35;1
# Bright Cyan:    36;1
# Bright White:   37;1

function debug(text)   { return magenta(text) }
function error(text)   { return red(text)     }
function info(text)    { return blue(text)    }
function warning(text) { return yellow(text)  }

function print_debug(text) {
    if (LOG_DEBUG)
        print debug("[DEBUG] ") text
}

function print_debug_array(a) {
    for (i in a) {
        # if (awk::isarray(a[i])) {
        #     print_debug_array(a[i])
        # }
        # else {
        print_debug("[" i "]: '" a[i] "'")
        # }
    }
}

function get_uname_system_type(_system_type,
                                _line) {
    uname_macos_regex = @/(darwin)/
    uname_msys_regex = @/(mingw|msys)/
    uname = ""

    while (("uname -a" |& getline _line) > 0) {
        uname = _line
    }
    close("uname -a")
    print_debug("uname = " uname)
    _system_type = "gnu/linux"
    if (uname ~ uname_msys_regex)
        _system_type = "windows-nt"
    else if (uname ~ uname_macos_regex)
        _system_type = "darwin"
    _system_type = _trim_whitespace(_system_type)
    print_debug("uname_system_type = " _system_type)
    return _system_type
}

function get_hostname(_hostname,
                       _line) {
    while (("hostname" |& getline line) > 0) {
        _hostname = line
    }
    close("hostname")
    _hostname = _trim_whitespace(_hostname)
    print_debug("hostname = " _hostname)
    return _hostname
}

function _trim_whitespace(text,
                         _trimmed_text) {
    trimmed_text = text
    sub(/^\s+/, "", trimmed_text)
    sub(/\s+$/, "", trimmed_text)
    return trimmed_text
}
