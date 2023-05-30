#!/usr/bin/gawk -f
@include "assert"
@include "path"
@include "cli"

BEGIN {
    assert::set_keep_going()

    run_tests()

    exit assert::get_exit_code()
}

function run_tests() {
    assert::NotEmpty(path::cwd())

    assert::Equal(path::basename("/some/file/name"), "name")
    assert::Equal(path::dirname("/some/file/name"), "/some/file")
    starting_dir = path::cwd()

    path::mkdirp("testdir/dir1/dir2")

    path::pushd("testdir")
    assert::Equal(length(path::dirstack), 2)
    assert::Equal(path::dirstack[1], starting_dir)

    path::pushd("dir1")
    assert::Equal(length(path::dirstack), 3)
    assert::Equal(path::dirstack[2], starting_dir "/testdir")

    path::pushd("dir2")
    assert::Equal(length(path::dirstack), 4)
    assert::Equal(path::dirstack[3], starting_dir "/testdir/dir1")

    assert::Equal(path::popd(), starting_dir "/testdir/dir1/dir2")
    assert::Equal(length(path::dirstack), 3)

    assert::Equal(path::popd(), starting_dir "/testdir/dir1")
    assert::Equal(length(path::dirstack), 2)

    assert::Equal(path::popd(), starting_dir "/testdir")
    assert::Equal(length(path::dirstack), 1)
    assert::Equal(path::dirstack[0], "")

    system("rm -rf testdir")
    print ""

    cli::LOG_DEBUG = 1
    # Print all path namespace variables
    cli::print_debug_array(SYMTAB, "path::")
    path::glob("..", "\.org$")

    # Traverse array ordered by indices in ascending order compared as strings
    _original_sort = PROCINFO["sorted_in"]
    PROCINFO["sorted_in"] = "@ind_str_asc"

    for (f in path::globdata_flattened)
        print path::globdata_flattened[f]

    PROCINFO["sorted_in"] = _original_sort

    cli::print_debug_array(SYMTAB, "path::")

    assert::Equal(path::basename(path::which("gawk")), "gawk")
    assert::Equal(path::which("SomeNonExistentGawkPathTangleAwkTest"), "")
}
