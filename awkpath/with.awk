#!/usr/bin/gawk -f
@namespace "with"

function sorting(sort_mode, function_to_run,
                 _result) {
    # Indices in ascending order compared as strings
    _original_sort = PROCINFO["sorted_in"]
    PROCINFO["sorted_in"] = sort_mode

    _result = @function_to_run()

    PROCINFO["sorted_in"] = _original_sort
    return _result
}

function sort_index_string_asc(function_to_run) {
    return sorting("@ind_str_asc", function_to_run)
}


