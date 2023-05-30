#!/usr/bin/gawk -f
@load "filefuncs"
@namespace "path"

function basename(path,
                  _path_array) {
    split(path, _path_array, "/")
    return _path_array[length(_path_array)]
}

function dirname(path,
                 _path_array) {
    split(path, _path_array, "/")
    return _join(_path_array, 1, length(_path_array)-1, "/")
}

function pushd(dir_path) {
    dirstack[0] = ""
    dirstack[length(dirstack)] = cwd()
    # for (i in dirstack)
    #     print "("length(dirstack)") " i ": " dirstack[i]
    awk::chdir(dir_path)
}

function popd(_last_dir) {
    dirstack[0] = ""
    # for (i in dirstack)
    #     print "("length(dirstack)") " i ": " dirstack[i]
    if (length(dirstack) > 1) {
        _last_dir = cwd()
        awk::chdir(dirstack[length(dirstack) - 1])
        delete dirstack[length(dirstack) - 1]
        return _last_dir
    }
}

function mkdirp(target_path) {
    system("mkdir -p " target_path)
}

function cwd(_current_dir) {
    "pwd" | getline _current_dir
    close("pwd")
    return _current_dir
}

function resolve(target_path,
                 _resolved_path, _command) {
    _command = "readlink -f " target_path
    _command | getline _resolved_path
    close(_command)
    return _resolved_path
}

function is_file(target_path,
                 _result, _fdata) {
    _result = awk::stat(target_path, _fdata)
    # Error with stat()
    if (_result < 0)
        return 0
    return _fdata["type"] == "file"
 }

function is_dir(target_path,
                _result, _fdata) {
    _result = awk::stat(target_path, _fdata)
    # Error with stat()
    if (_result < 0)
        return 0
    return _fdata["type"] == "directory"
 }


function which(bin_name,
               _command, _result) {
    _result = ""
    _command = "which " bin_name
    _command | getline _result
    close(_command)
    return _result
 }

function md5sum(target_path,
                _md5_bin, _result_output,
                _result_index, _result_array, _command) {
    _md5_bin = ""
    _result_index = 0
    if (which("md5sum")) {
        _md5_bin = "md5sum"
        _result_index = 1
    }
    else if (which("md5")) {
        _md5_bin = "md5"
        _result_index = 4
    }

    _command = _md5_bin " " target_path
    _command | getline _result_output
    close(_command)
    split(_result_output, _result_array, " ")

    return _result_array[_result_index]
}

function glob(target_path,
              pattern,
              _file,
              _pathlist, _flags, _result,
              _original_sort) {

    _pathlist[0] = target_path
    _result = awk::fts(_pathlist,
                       awk::FTS_LOGICAL,
                       globdata)

    _original_sort = PROCINFO["sorted_in"]
    # Traverse array ordered by indices in ascending order compared as strings
    PROCINFO["sorted_in"] = "@ind_str_asc"

    delete globdata_flattened
    globdata_pattern = ""
    if (pattern) {
        globdata_pattern = pattern
    }

    process_array(globdata[target_path], "", "path::save_glob_file", 0)

    PROCINFO["sorted_in"] = _original_sort
}

function save_glob_file(varname, element) {
    if (globdata_pattern == "") {
        globdata_flattened[varname] = element
    }
    else if (varname ~ globdata_pattern) {
        globdata_flattened[varname] = element
    }
}

function process_array(arr, name, process, do_arrays,
                       i, new_name) {
    for (i in arr) {
        new_name = (name i "/")

        # Skip . which represents the current
        if (i == ".")
            continue

        if (awk::isarray(arr[i])) {

            if ("path" in arr[i])
                @process(name i, arr[i]["path"])

            # TODO: depth check here

            # if (do_arrays)
            #     @process(new_name, arr[i])

            if (length(arr[i]) >= 2 && length(arr[i]) <= 3 && ("path" in arr[i]) && ("stat" in arr[i])) {
            }
            else {
                process_array(arr[i], new_name, process, do_arrays)
            }
        }
        else {
            @process(new_name, arr[i])
        }
    }
}


# Private functions

# Join an array into a string (ignoring empty strings)
function _join(array, start, end, sep,
               _result, _i) {
    if (sep == "")
       sep = " "
    else if (sep == SUBSEP) # magic value
       sep = ""
    _result = array[start]
    for (_i = start + 1; _i <= end; _i++)
        if (array[_i] != "")
            _result = _result sep array[_i]
    return _result
}
