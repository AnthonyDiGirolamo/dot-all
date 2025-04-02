#!/usr/bin/gawk -f
@include "tangle"
@include "assert"
@include "cli"

BEGIN {
    assert::set_keep_going()

    run_tests()

    exit assert::get_exit_code()
}

function run_tests() {
    assert::Equal(tangle::_get_uname_system_type(),
                  "gnu/linux")
    assert::Equal(tangle::trim_whitespace("  something   "),
                  "something")
    assert::Equal(tangle::trim_whitespace("  \nhello\nthere\n\n "),
                  "hello\nthere")

    assert::Equal(tangle::dirname("/c/Users/test/something"),
                  "/c/Users/test")
    assert::Equal(tangle::basename("/c/Users/test/something"),
                  "something")
    print ""

    cli::LOG_DEBUG = 1
    # Print all path namespace variables
    cli::print_debug_array(SYMTAB, "tangle::")
}


