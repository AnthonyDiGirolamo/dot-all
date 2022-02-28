#!/usr/bin/gawk -f
@include "tangle"
@include "assert"

BEGIN {
    _assert_exit = 0

    assert::Equal(trim_whitespace("  something   "),
                  "something")
    assert::Equal(trim_whitespace("  \nhello\nthere\n\n "),
                  "hello\nthere")

    assert::Equal(dirname("/c/Users/test/something"),
                  "/c/Users/test")
    assert::Equal(basename("/c/Users/test/something"),
                  "something")

    printf "\n" > "/dev/stderr"
    exit _assert_exit
}


