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
    return join(_path_array, 1, length(_path_array)-1, "/")
}

function pushd(dir_path) {
    dirstack[0] = ""
    dirstack[length(dirstack)] = cwd()
    # for (i in dirstack)
    #     print "("length(dirstack)") " i ": " dirstack[i]
    awk::chdir(dir_path)
}

function popd() {
    dirstack[0] = ""
    # for (i in dirstack)
    #     print "("length(dirstack)") " i ": " dirstack[i]
    if (length(dirstack) > 1) {
        awk::chdir(dirstack[length(dirstack) - 1])
        delete dirstack[length(dirstack) - 1]
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
    _command = "realpath " target_path
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

 function md5sum(target_path,
                 _result_hash) {
    _command = "md5sum " target_path
    _command | getline _result_hash
    close(_command)
    return _result_hash
 }

 function md5sum_matches(target_path, expected_hash,
                         _result, _matches) {
     _matches = 0
     _result = md5sum(target_path)
     if (_result == expected_hash "  " target_path)
         _matches = 1
     return _matches
 }

