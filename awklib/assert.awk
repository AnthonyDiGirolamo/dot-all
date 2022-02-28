#!/usr/bin/gawk -f
@namespace "assert"

# assert::assert --- assert that a condition is true. Otherwise, exit.
# https://www.gnu.org/software/gawk/manual/gawk.html#Assert-Function
function assert(condition, string)
{
    if (! condition) {
        printf("\n%s:%d: assertion failed: %s\n",
            FILENAME, FNR, string) > "/dev/stderr"
        _exit_code = 1
        if (!keep_going)
            exit 1
    }
    else {
        printf "." > "/dev/stderr"
    }
}

function set_keep_going() {
    keep_going = 1
}

function get_exit_code() {
    if (_exit_code == 1)
        return 1
    else
        return 0
}

# Create human readable string representations of variables
function _human_readable(var) {
    if (awk::typeof(var) != "string")
        return "" var
    else
        return "\"" var "\""
}

# assert::Equal --- assert two expressions are equal.
function Equal(result, expected,
               _result_string, _expected_string) {
    _result_string = _human_readable(result)
    _expected_string = _human_readable(expected)

    # Check equal types
    assert(awk::typeof(result) == awk::typeof(expected),
           "typeof(" _result_string ") == typeof(" _expected_string ")")
    assert(result == expected,
           _result_string " == " _expected_string)
}

# assert::NotEmpty --- assert variable is not an empty string
function NotEmpty(result) {
    assert(result != "",
           _human_readable(result) " != \"\"")
}
