#!/usr/bin/gawk -f
@namespace "cli"

function color(colorcode, text) {
    if (ENVIRON["TERM"] == "dumb")
        return text
    else
        return sprintf("\033[%dm%s\033[0m", colorcode, text)
}

function green(text) { return color(32, text) }
function cyan(text) { return color(36, text) }
function warning(text) { return color(33, text) }
function error(text) { return color(31, text) }
function debug(text) { return color(35, text) }

# Black:   30
# Red:     31
# Green:   32
# Yellow:  33
# Blue:    34
# Magenta: 35
# Cyan:    36
# White:   37
# Bright Black:   30;1
# Bright Red:     31;1
# Bright Green:   32;1
# Bright Yellow:  33;1
# Bright Blue:    34;1
# Bright Magenta: 35;1
# Bright Cyan:    36;1
# Bright White:   37;1
# Reset:   0

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
    _system_type = trim_whitespace(_system_type)
    print_debug("uname_system_type = " _system_type)
    return _system_type
}

function get_hostname(_hostname,
                       _line) {
    while (("hostname" |& getline line) > 0) {
        _hostname = line
    }
    close("hostname")
    _hostname = trim_whitespace(_hostname)
    print_debug("hostname = " _hostname)
    return _hostname
}

function trim_whitespace(text,
                         _trimmed_text) {
    trimmed_text = text
    sub(/^\s+/, "", trimmed_text)
    sub(/\s+$/, "", trimmed_text)
    return trimmed_text
}
