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
    if (ENVIRON["TERM"] == "dumb")
        printf "[%s] %s\n", tag, text
    else
        printf "\033[36m[%s]\033[0m %s\n", tag, text
}

function print_array_indexes(a) {
    for (i in a)
        print "["i"]: "
}

function find_index_ending_in(pattern, ary) {
    # check for patterns matching the end of the line
    p = pattern "$"
    print p
    for (i in ary)
        if (i ~ p)
            return i
    return 0
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

function start_new_file() {
    print_tag_line("TANGLE", FILENAME)

    # per file vars
    tangle_prop_file_name = 0
    eval_block_count = 0
    total_block_count = 0
    delete tangled_files

    reset_block_variables()
}

function reset_block_variables () {
    # per block vars
    in_block = 0
    current_block_line_number = 0
    current_block_filename = 0
    current_block_indent = 0
}

function start_new_block() {
    reset_block_variables()
    in_block = 1
    total_block_count += 1
}

function tangle_file_name () {
    if (current_block_filename)
        return current_block_filename
    else if (tangle_prop_file_name)
        return tangle_prop_file_name
    else
        return 0
}

function make_block_name(count, name) {
    # Check for existing block name and use that if found
    # (so lines can continue to be appended to that file)
    existing_name = find_index_ending_in(name, tangled_files)
    if (existing_name) {
        # print "found index " existing_name
        return existing_name
    }
    # print "new index for " name
    return sprintf("BLOCK%03d %s", count, name)
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

    org_escaped_asterix_regex = @/^(\s*,[*])/

    # (if (file-exists-p "~/.gitconfig") "no" "~/.gitconfig")
    elisp_file_exists_p_regex = @/\(if\s*\(file-exists-p\s*"([^"]+)"\)\s*"([^"]+)"\s*"([^"]+)"\s*\)/
    # (if (string-match "chip" hostname) "~/.i3/config" "no")
    elisp_string_suffix_p_regex = @/\(if\s*\(string-suffix-p\s*"([^"]+)"\s*([^)]+)\)\s*"([^"]+)"\s*"([^"]+)"\s*\)/

}

BEGINFILE {
    start_new_file()
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
    reset_block_variables()
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
        # check for an escaped asterix and remove the comma
        if (match(current_line, org_escaped_asterix_regex, mg)) {
            replacement = mg[1]
            sub(/,[*]/, "*", replacement)
            sub(org_escaped_asterix_regex, replacement, current_line)
        }
        # append the line and a linebreak
        tangled_files[tangle_file_name()] = tangled_files[tangle_file_name()] "\n" current_line
        # print current_block_line_number,":",current_line
    }
}

# Check for start block line
#   Should come after in_block so the end_src line isn't printed
match($0, begin_src_regex, group) {
    start_new_block()
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
            if (! match($0, /:noweb/)) {
                current_block_filename = make_block_name(total_block_count, file_name)
            }
    }
}

# Check for an sh block with :eval yes
match($0, begin_src_sh_eval_regex, group) {
    start_new_block()
    eval_block_count += 1
    current_block_filename = make_block_name(total_block_count, sprintf("eval-block-sh %03d", eval_block_count))
}

ENDFILE {
    outfile = ".cache/" FILENAME
    sub(/\.org$/, ".out", outfile)
    # Move existing .out file to .out.last
    print "test -f "outfile" && cp "outfile" "outfile".last && rm "outfile | "sh"
    close("sh")

    # Traverse array ordered by indices in ascending order compared as strings
    PROCINFO["sorted_in"] = "@ind_str_asc"

    # print_array_indexes(tangled_files)
    for (file_name in tangled_files) {
        # run a script case
        if (match(file_name, /(BLOCK[0-9]+) eval-block-sh/)) {
            printf "  "
            print_tag_line("RUNSCRIPT", "sh")
            print tangled_files[file_name] | "sh"
            close("sh")
        }
        # file tangle case
        else if (match(file_name, /^(BLOCK[0-9]+) (.*)$/, mg)) {
            # capture block prefix and filename separately
            block_prefix = mg[1]
            expanded_file_name = mg[2]

            # If file name doesn't start with:
            #   (  -> isn't an elisp expression
            #   no -> should not be tangled
            if (! match(expanded_file_name, /^\s*("?no"?|\()/, mg)) {
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
            # elisp file-exists-p call
            else if (match(expanded_file_name, elisp_file_exists_p_regex, mg)) {
                existing_file = mg[1]
                true_case = mg[2]
                false_case = mg[3]
                # print "file-exists-p", existing_file, true_case, false_case
            }
            # elisp string-suffix-p call
            else if (match(expanded_file_name, elisp_string_suffix_p_regex, mg)) {
                pattern = mg[1]
                variable = mg[2]
                true_case = mg[3]
                false_case = mg[4]
                # print "string-suffix-p", pattern, variable, true_case, false_case
            }
        }
    }
    close(outfile)
}
