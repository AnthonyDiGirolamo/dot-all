#!/usr/bin/gawk -f
@include "assert"
@include "cli"

BEGIN {
    assert::set_keep_going()

    print_test()
    run_tests()

    exit assert::get_exit_code()
}

function run_tests() {
    assert::NotEmpty(cli::get_uname_a())
    assert::NotEmpty(cli::get_hostname())
    assert::NotEmpty(cli::get_uname_system_type())
    print ""

    cli::LOG_DEBUG = 1
    # Print all cli namespace variables
    cli::print_debug_array(SYMTAB, "cli::")
}

function print_test() {
    printf "Colors: "
    text = "#"
    printf cli::black(text)
    printf cli::red(text)
    printf cli::green(text)
    printf cli::yellow(text)
    printf cli::blue(text)
    printf cli::magenta(text)
    printf cli::cyan(text)
    printf cli::white(text)
    printf "\n"
    printf "Bright: "
    text = "#"
    printf cli::bright_black(text)
    printf cli::bright_red(text)
    printf cli::bright_green(text)
    printf cli::bright_yellow(text)
    printf cli::bright_blue(text)
    printf cli::bright_magenta(text)
    printf cli::bright_cyan(text)
    printf cli::bright_white(text)
    printf "\n"

    print("Log levels:",
          cli::debug("DBG"),
          cli::error("ERR"),
          cli::info("INF"),
          cli::warning("WRN"),
          cli::success("OK!"))

    # 256 Color Test
    print "256 Color Codes"
    for (i=0; i<256; i++) {
        printf cli::bg_color_256(i) " %03d ", i
        if ((i+1) % 16 == 0) {
            printf cli::reset() "\n"
        }
    }
    printf cli::reset()
}
