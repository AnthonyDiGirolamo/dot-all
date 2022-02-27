#!/usr/bin/gawk -f
@include "tangle"

# assert --- assert that a condition is true. Otherwise, exit.
# https://www.gnu.org/software/gawk/manual/gawk.html#Assert-Function
function assert(condition, string)
{
    if (! condition) {
        printf("\n%s:%d: assertion failed: %s\n",
            FILENAME, FNR, string) > "/dev/stderr"
        _assert_exit = 1
        # exit 1
    }
    else {
        printf "." > "/dev/stderr"
    }
}

# assertEqual --- assert two expressions are equal.
function assertEqual(result, expected) {
    assert(result == expected,
           "\"" result "\" != \"" expected "\"")
}

BEGIN {
    _assert_exit = 0

    assertEqual(trim_whitespace("  something   "),
                "something")
    assertEqual(trim_whitespace("  \nhello\nthere\n\n "),
                "hello\nthere")

    assertEqual(dirname("/c/Users/test/something"),
                "/c/Users/test")
    assertEqual(basename("/c/Users/test/something"),
                "something")

    printf "\n" > "/dev/stderr"
    exit _assert_exit
}


