function init_block () {
    in_block = 0
    current_block_line_number = 0
    current_block_filename = 0
    current_block_indent = 0
}

function tangle_file_name () {
    if (current_block_filename)
        return current_block_filename
    else if (tangle_prop_file_name)
        return tangle_prop_file_name
    else
        return 0
}

BEGIN {
    IGNORECASE = 1
    tangle_prop_regex = @/#\+property.*header-args.*:tangle ([^ ]+)/
    begin_src_regex = @/#\+begin_src/
    begin_src_tangle_regex = @/#\+begin_src.*:tangle ([^:]+)/
    end_src_regex = @/#\+end_src/

    init_block()
    tangle_prop_file_name = 0
}

# check for a header-args :tangle property and save the filename
# E.g. #+PROPERTY: header-args :tangle "~/.emacs.d/README.el"
match($0, tangle_prop_regex, group) {
    tangle_prop_file_name = group[1]
    print tangle_prop_file_name
}

# should come before in_block so the end_src line isn't printed
match($0, end_src_regex) {
    init_block()
}

in_block {
    current_line = $0
    current_block_line_number += 1
    # get starting indent length on the first line
    if (current_block_line_number == 1) {
        # capture leading spaces substring
        if (match(current_line, /^(\s+)\S/, spacegroup)) {
            current_block_indent = spacegroup[1]
            # print length(current_block_indent), "[", current_block_indent, "]"
        }
    }
    # if there is a filename and it isn't no
    if (tangle_file_name() && tangle_file_name() != "no") {
        # remove leading indentation
        sub(current_block_indent, "", current_line)
        # print current_block_line_number,":",current_line
    }
}

# start block line
# should come after in_block so the end_src line isn't printed
match($0, begin_src_regex, group) {
    init_block()
    in_block = 1
    # with tangle?
    if (match($0, begin_src_tangle_regex, tanglegroup)) {
        if (tanglegroup[1] == "no" && tanglegroup[1] == "nil")
            current_block_filename = "no"
        else {
            current_block_filename = tanglegroup[1]
            # remove leading and trailing quotes
            sub(/^["]/, "", current_block_filename)
            sub(/["]$/, "", current_block_filename)
        }
    }
    if (tangle_file_name()) {
        print tangle_file_name()
    }
}
