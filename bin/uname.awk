#!/usr/bin/gawk -f

BEGIN {
    IGNORECASE = 1
    # Example uname -a output
    # MINGW64_NT-10.0-19043 hostname 3.0.7-338.x86_64 2019-11-21 23:07 UTC x86_64 Msys
    uname_macos_regex = @/(darwin)/
    uname_msys_regex = @/(mingw|msys)/
    uname = ""

    while (("uname -a" |& getline line) > 0) {
        uname = line
    }
    close("uname -a")

    platform = "linux"
    if (uname ~ uname_msys_regex)
        platform = "windows"
    else if (uname ~ uname_macos_regex)
        platform = "macos"

    print platform
}
