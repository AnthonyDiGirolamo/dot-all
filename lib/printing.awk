#!/usr/bin/gawk -f

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

function print_tag_line(tag, text) {
    if (ENVIRON["TERM"] == "dumb")
        print "[" tag "]", text
    else
        print wrap_cli_color(36, "[" tag "]"), text
}

function _DEBUG(text) {
    if (LOG_DEBUG)
        print cli_debug("[DEBUG] ") text
}

function _DEBUG_ARRAY(a) {
    for (i in a)
        _DEBUG("[" i "]: '" a[i] "'")
}

function _get_uname_system_type(_system_type,
                                _line) {
    uname_macos_regex = @/(darwin)/
    uname_msys_regex = @/(mingw|msys)/
    uname = ""

    while (("uname -a" |& getline _line) > 0) {
        uname = _line
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

function _get_hostname(_hostname,
                       _line) {
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
