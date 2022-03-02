#!/usr/bin/gawk -f
@namespace "cli"

function wrap_color(colorcode, text) {
    if (ENVIRON["TERM"] == "dumb")
        return text
    else
        return sprintf("\x1b[%dm%s\x1b[0m", colorcode, text)
}


function fg_color_256(color_index) { return "\x1b[38;5;" color_index "m" }
function bg_color_256(color_index) { return "\x1b[48;5;" color_index "m" }

function reset() { return "\x1b[m" }

function black(text)   { return wrap_color(30, text) }
function red(text)     { return wrap_color(31, text) }
function green(text)   { return wrap_color(32, text) }
function yellow(text)  { return wrap_color(33, text) }
function blue(text)    { return wrap_color(34, text) }
function magenta(text) { return wrap_color(35, text) }
function cyan(text)    { return wrap_color(36, text) }
function white(text)   { return wrap_color(37, text) }
function rgb(red, green, blue) { return "" }

function bright_black(text)   { return wrap_color("30;1", text) }
function bright_red(text)     { return wrap_color("31;1", text) }
function bright_green(text)   { return wrap_color("32;1", text) }
function bright_yellow(text)  { return wrap_color("33;1", text) }
function bright_blue(text)    { return wrap_color("34;1", text) }
function bright_magenta(text) { return wrap_color("35;1", text) }
function bright_cyan(text)    { return wrap_color("36;1", text) }
function bright_white(text)   { return wrap_color("37;1", text) }

function debug(text)   { return magenta(text) }
function error(text)   { return red(text)     }
function info(text)    { return blue(text)    }
function warning(text) { return yellow(text)  }
function success(text) { return green(text)   }

function print_debug(text) {
    if (LOG_DEBUG)
        print debug("[DEBUG] ") text
}

function print_debug_array(a, prefix,
                           _pattern, i) {
    if (!prefix)
        _pattern = ".*"
    else
        _pattern = "^"prefix".*"

    for (i in a) {
        if (i ~ _pattern) {
            if (awk::isarray(a[i])) {
                print_debug(i " = array, " length(a[i]) " elements")
                # print_debug_array(a[i])
            }
            else {
            print_debug(i ": '" a[i] "'")
            }
        }
    }
}

function get_uname_a(_result) {
    # Return previously fetched uname
    if (uname_a != "")
        return uname_a
    "uname -a" | getline _result
    close("uname -a")
    uname_a = _result
    return uname_a
}

function get_uname_system_type(system_type, uname,
                               uname_macos_regex, uname_msys_regex) {
    uname_macos_regex = @/(darwin)/
    uname_msys_regex = @/(mingw|msys)/
    uname = get_uname_a()

    system_type = "gnu/linux"
    if (uname ~ uname_msys_regex)
        system_type = "windows-nt"
    else if (uname ~ uname_macos_regex)
        system_type = "darwin"
    return system_type
}

function get_hostname(_result) {
    # Return previously fetched hostname
    if (hostname != "")
        return hostname
    "hostname" | getline _result
    close("hostname")
    hostname = _result
    return hostname
}

