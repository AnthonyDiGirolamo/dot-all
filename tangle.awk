#!/usr/bin/gawk -f

# join an array into a string (ignoring empty strings)
function join(array, start, end, sep, result, i) {
    if (sep == "")
       sep = " "
    else if (sep == SUBSEP) # magic value
       sep = ""
    result = array[start]
    for (i = start + 1; i <= end; i++)
        if (array[i] != "")
            result = result sep array[i]
    return result
}

function print_tag_line(tag, text) {
    printf "\033[36m[%s]\033[0m %s\n", tag, text
}

function print_array_indexes(a) {
    for (i in a)
        print "["i"]: "
}

function print_array(a) {
    for (i in a)
        print "["i"]: '"a[i]"'"
}

function basename(path) {
    split(path, path_array, "/")
    return path_array[length(path_array)]
}

function dirname(path) {
    split(path, path_array, "/")
    return join(path_array, 1, length(path_array)-1, "/")
}

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
    tangle_prop_regex = @/^\s*#\+property.*header-args.*:tangle\s*(\S.*)$/
    begin_src_regex = @/^\s*#\+begin_src/
    # need both variants as awk doesn't seem to have non-greedy regexes
    begin_src_tangle_regex = @/^\s*#\+begin_src.*:tangle\s*(\S.*) :/
    begin_src_tangle_to_end_regex = @/^\s*#\+begin_src.*:tangle\s*(\S.*)$/
    begin_src_sh_eval_regex = @/^\s*#\+begin_src sh .*:eval "?yes"?/
    end_src_regex = @/^\s*#\+end_src/
}

BEGINFILE {
    print_tag_line("TANGLE", FILENAME)
    init_block()
    tangle_prop_file_name = 0
    eval_block_count = 0
    delete tangled_files
}

# Check for a header-args :tangle property and save the filename
# E.g. #+PROPERTY: header-args :tangle "~/.emacs.d/README.el"
match($0, tangle_prop_regex, group) {
    tangle_prop_file_name = group[1]
    # init the file contents
    tangled_files[tangle_file_name()] = ""
}

# Check for an end block line
#   Should come before in_block so the end_src line isn't printed
match($0, end_src_regex) {
    if (in_block && tangle_file_name())
        # output one extra line break for this block
        tangled_files[tangle_file_name()] = tangled_files[tangle_file_name()] "\n"
    # start a new block
    init_block()
}

# If we are inside a src block, capture the current line
in_block {
    current_line = $0
    current_block_line_number += 1
    # get starting indent length on the first line with text
    if (!current_block_indent) {
        # capture leading spaces substring
        if (match(current_line, /^(\s+)\S/, spacegroup))
            current_block_indent = spacegroup[1]
    }
    # if there is a filename and it isn't no
    if (tangle_file_name()) {
        # remove leading indentation
        sub(current_block_indent, "", current_line)
        # append the line and a linebreak
        tangled_files[tangle_file_name()] = tangled_files[tangle_file_name()] "\n" current_line
        # print current_block_line_number,":",current_line
    }
}

# Check for start block line
#   Should come after in_block so the end_src line isn't printed
match($0, begin_src_regex, group) {
    init_block()
    in_block = 1
    # with tangle?
    if (match($0, begin_src_tangle_regex, tanglegroup) || match($0, begin_src_tangle_to_end_regex, tanglegroup)) {
        file_name = tanglegroup[1]
        # # trim whitespace
        # sub(/^\s+/, "", file_name)
        # sub(/\s+$/, "", file_name)
        # don't collect lines for :tangle "no" blocks
        if (match(file_name, /^['"]?(no|nil)['"]?/))
            in_block = 0
        else
            # can't handle noweb references yet
            if (! match($0, /:noweb/))
                current_block_filename = file_name
    }
}

# Check for an sh block with :eval yes
match($0, begin_src_sh_eval_regex, group) {
    init_block()
    in_block = 1
    eval_block_count += 1
    current_block_filename = "eval-block-sh-"eval_block_count
}

ENDFILE {
    outfile = ".cache/" FILENAME
    sub(/\.org$/, ".out", outfile)
    # Move existing .out file to .out.last
    print "test -f "outfile" && cp "outfile" "outfile".last && rm "outfile | "sh"
    close("sh")

    # Traverse array ordered by indices in ascending order compared as strings
    # PROCINFO["sorted_in"] = "@ind_str_asc"
    # Need to traverse in original order in case any sh blocks expect previously
    # tangled files to exist.

    # print_array_indexes(tangled_files)
    for (file_name in tangled_files) {
        if (match(file_name, /eval-block-sh-/)) {
            printf "  "
            print_tag_line("RUNSCRIPT", "sh")
            print tangled_files[file_name] | "sh"
            close("sh")
        }
        # If file name doesn't start with:
        #   (  -> isn't an elisp expression
        #   no -> should not be tangled
        else if (! match(file_name, /^\s*("?no"?|\()/)) {
            expanded_file_name = file_name
            # remove any leading and trailing quotes
            sub(/^["]/, "", expanded_file_name)
            sub(/["]$/, "", expanded_file_name)
            # expand ~ to $HOME
            sub(/~/, ENVIRON["HOME"], expanded_file_name)
            # print file being tangled
            print "  " expanded_file_name
            # always mkdir -p
            system("mkdir -v -p "dirname(expanded_file_name))
            # output contents string to the file all at once
            print tangled_files[file_name] > expanded_file_name
            close(expanded_file_name)

            # add tangled file path to outfile
            print expanded_file_name > outfile
        }
    }
    close(outfile)
}
