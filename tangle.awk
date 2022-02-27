#!/usr/bin/gawk -f
# Copyright 2021 Anthony DiGirolamo
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Usage: tangle.awk *.org
#
# This script which attempts to mimic Emacs `org-babel-tangle-file`.
#
# - It will copy each source block in a given `*.org` file into it's respective
#   `:tangle` destination and create any parent directories.
# - Some conditional checks are supported for :tangle and :eval:
#   - `(if (file-exists-p "~/.gitconfig") "no" "~/.gitconfig")`
#   - `(if (eq system-type 'windows-nt) "yes" "no")`
#     - Valid types: `'windows-nt`, `'gnu/linux`, `'darwin`
#   - `(if (string-suffix-p "chip" hostname) "~/.i3/config" "no")`
#     - Only supported variable is: `hostname`
# - Any blocks with `#+begin_src sh :eval yes` will be executed as well. The
#   above elisp checks are supported as well.
#
# Example Run with dotfiles from https://github.com/AnthonyDiGirolamo/dot-all
#
#   $ ./tangle.awk *.org
#   [TANGLE] bash.org
#     ~/.bashrc
#     ~/.bash_profile
#     ~/.aliases
#   [TANGLE] fish.org
#     ~/.config/fish/config.fish
#     ~/.config/fish/conf.d/aliases.fish
#     ~/.config/fish/conf.d/fish_user_key_bindings.fish
#     ~/.config/fish/functions/fish_prompt.fish
#     ~/.config/fish/functions/fish_mode_prompt.fish
#     ~/.config/fish/functions/tarxz.fish
#     ~/.config/fish/functions/u.fish
#     ~/.config/fish/functions/dl.fish
#     ~/.config/fish/functions/source-bash-aliases.fish
#     ~/.config/fish/completions/dhcpcd-restart.fish
#   [TANGLE] terminfo.org
#     .artifacts/terminfo-24bit.src
#     [RUNSCRIPT] sh
#     [PROCESS tic]
#   [TANGLE] tmux.org
#     ~/.tmux.conf
#   [TANGLE] zsh.org
#     ~/.zshrc
#     ~/.zshrc.local
#
# To lint this file, run:
#   gawk --lint=no-ext -f tangle.awk *.org

# Helper Fuctions

# Join an array into a string (ignoring empty strings)
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

# Terminal color wrap functions

function wrap_cli_color(colorcode, text) {
    if (ENVIRON["TERM"] == "dumb")
        return text
    else
        return sprintf("\033[%dm%s\033[0m", colorcode, text)
}
function cli_warning(text) {
    return wrap_cli_color(33, text)
}
function cli_error(text) {
    return wrap_cli_color(31, text)
}
function cli_debug(text) {
    return wrap_cli_color(35, text)
}

# Print a log message if TANGLEAWK_LOG is env var is set.
function _DEBUG(text) {
    if (LOG_DEBUG)
        print cli_debug("[DEBUG] ") text
}

function print_tag_line(tag, text) {
    if (ENVIRON["TERM"] == "dumb")
        print "[" tag "]", text
    else
        print wrap_cli_color(36, "[" tag "]"), text
}

function find_index_ending_in(pattern, ary) {
    # check for patterns matching the end of the line
    p = pattern "$"
    for (i in ary)
        if (i ~ p)
            return i
    return 0
}

# function print_array_indexes(a) {
#     for (i in a)
#         print "["i"]: "
# }

# function print_array(a) {
#     for (i in a)
#         print "["i"]: '"a[i]"'"
# }

function _DEBUG_ARRAY(a) {
    for (i in a)
        _DEBUG("[" i "]: '" a[i] "'")
}

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

function trim_whitespace(text,
                         _trimmed_text) {
    trimmed_text = text
    sub(/^\s+/, "", trimmed_text)
    sub(/\s+$/, "", trimmed_text)
    return trimmed_text
}

function _get_uname_system_type(_system_type) {
    while (("uname -a" |& getline line) > 0) {
        uname = line
    }
    close("uname -a")
    _DEBUG("uname = " uname)
    _system_type = "gnu/linux"
    if (uname ~ uname_msys_regex)
        _system_type = "windows-nt"
    else if (uname ~ uname_macos_regex)
        _system_type = "darwin"
    _system_type = trim_whitespace(_system_type)
    _DEBUG("uname_system_type = " _system_type)
    return _system_type
}

function _get_hostname(_hostname) {
    while (("hostname" |& getline line) > 0) {
        _hostname = line
    }
    close("hostname")
    _hostname = trim_whitespace(_hostname)
    _DEBUG("hostname = " _hostname)
    return _hostname
}

# Tangle.awk Specific Functions
function start_new_file() {
    print_tag_line("TANGLE", FILENAME)

    # per file vars
    tangle_prop_file_name = 0
    eval_block_count = 0
    total_block_count = 0
    delete tangled_files
    delete block_conditions

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
        # erint "found index " existing_name
        return existing_name
    }
    # print "new index for " name
    return sprintf("BLOCK%03d %s", count, name)
}

function parse_tangle_or_eval_file_expression(mode, file_expression, result_group) {
    # Reset result_group values
    result_group[FILE_EXPRESSION] = ""
    result_group[CONDITION_TYPE] = ""
    result_group[CONDITION_DATA1] = ""
    result_group[CONDITION_DATA2] = ""
    result_group[CONDITION_TRUE_CASE] = ""
    result_group[CONDITION_FALSE_CASE] = ""

    if (match(file_expression, double_quoted_value_regex, match_group)) {
        result_group[FILE_EXPRESSION] = match_group[0]
        result_group[CONDITION_TYPE] = "raw_value"
        result_group[CONDITION_TRUE_CASE]  = match_group[1]
        result_group[CONDITION_FALSE_CASE] = match_group[1]
    }
    else if (match(file_expression, elisp_system_type_regex, match_group)) {
        result_group[FILE_EXPRESSION] = match_group[0]
        result_group[CONDITION_TYPE] = "system_type"
        result_group[CONDITION_DATA1] = match_group[1]  # windows-nt or gnu/linux
        result_group[CONDITION_DATA2] = ""  # No condition data 2
        result_group[CONDITION_TRUE_CASE]  = match_group[2]  # true case
        result_group[CONDITION_FALSE_CASE] = match_group[3]  # false case
    }
    else if (match(file_expression, elisp_file_exists_p_regex, match_group)) {
        result_group[FILE_EXPRESSION] = match_group[0]
        result_group[CONDITION_TYPE] = "file_exists_p"
        result_group[CONDITION_DATA1] = match_group[1]
        result_group[CONDITION_DATA2] = ""  # No condition data 2
        result_group[CONDITION_TRUE_CASE]  = match_group[2]
        result_group[CONDITION_FALSE_CASE] = match_group[3]
    }
    else if (match(file_expression, elisp_string_suffix_p_regex, match_group)) {
        result_group[FILE_EXPRESSION] = match_group[0]
        result_group[CONDITION_TYPE] = "string_suffix_p"
        result_group[CONDITION_DATA1] = match_group[1]  # prefix text
        result_group[CONDITION_DATA2] = match_group[2]  # variable
        result_group[CONDITION_TRUE_CASE]  = match_group[3]  # true case
        result_group[CONDITION_FALSE_CASE] = match_group[4]  # false case
    }
    else if (match(file_expression, yes_or_no_regex, match_group)) {
        result_group[FILE_EXPRESSION] = match_group[0]
        result_group[CONDITION_TYPE] = "raw_value"
        result_group[CONDITION_TRUE_CASE]  = match_group[1]
        result_group[CONDITION_FALSE_CASE] = match_group[1]
    }
    else if (match(file_expression, unquoted_value_regex, match_group)) {
        result_group[FILE_EXPRESSION] = match_group[0]
        result_group[CONDITION_TYPE] = "raw_value"
        result_group[CONDITION_TRUE_CASE]  = match_group[1]
        result_group[CONDITION_FALSE_CASE] = match_group[1]
    }
    else {
        print "----------------------- "
        print cli_warning("[WARNING] Unknown " mode " format: ") file_expression
        print "  " file_expression
        print "----------------------- "
    }
    # _DEBUG_ARRAY(result_group)
}

# function get_parsed_file_expression(mode, text) {
#     parse_tangle_or_eval_file_expression(mode, text, result_group)
#     return result_group[FILE_EXPRESSION]
# }

function save_block_filename_condition(block_filename, result_group) {
    for (i in result_group) {
        block_conditions[block_filename][i] = result_group[i]
    }
}

function handle_tangle_or_eval_line(src_line) {
    # :tangle case
    if (match(src_line, begin_src_tangle_to_end_regex, tanglegroup)) {
        # _DEBUG("TANGLE LINE: " src_line)
        parse_tangle_or_eval_file_expression("tangle", tanglegroup[1], result_group)
        file_expression = result_group[FILE_EXPRESSION]

        # Don't collect lines for :tangle "no" blocks
        if (match(file_expression, no_or_nil_regex))
            return
        if (file_expression == "")
            return

        # Can't handle noweb references yet
        if (match(src_line, /:noweb/)) {
            print cli_warning("[WARNING] :noweb references are not handled.")
            print cli_warning("[WARNING] " FILENAME ":" FNR ":"), src_line
        }

        start_new_block()
        # _DEBUG("TANGLE FILE: " file_expression)
        current_block_filename = make_block_name(total_block_count,
                                                 file_expression)
        save_block_filename_condition(current_block_filename, result_group)
    }

    # :eval case
    else if (match(src_line, begin_src_eval_to_end_regex, tanglegroup)) {
        # _DEBUG("EVAL LINE: " src_line)
        shell_type = tanglegroup[1]
        parse_tangle_or_eval_file_expression("eval", tanglegroup[2], result_group)
        file_expression = result_group[FILE_EXPRESSION]
        if (file_expression == "")
            return

        start_new_block()
        # _DEBUG("EVAL-" shell_type ": " file_expression)
        eval_block_count += 1
        eval_block_title = sprintf("eval-%03d-" shell_type, eval_block_count)
        current_block_filename = make_block_name(total_block_count,
                                                 eval_block_title " " file_expression)
        save_block_filename_condition(current_block_filename, result_group)
    }
    else {
        print cli_error("[ERROR] Unknown src block format.")
        print cli_error("[ERROR] " FILENAME ":" FNR ":"), src_line
        # TODO: exit here?
    }
}

BEGIN {
    LOG_DEBUG = 0
    DRYRUN = 0
    if ("TANGLEAWK_LOG" in ENVIRON &&
        ENVIRON["TANGLEAWK_LOG"] != "")
        LOG_DEBUG = 1
    if ("TANGLEAWK_DRYRUN" in ENVIRON &&
        ENVIRON["TANGLEAWK_DRYRUN"] != "")
        DRYRUN = 1

    IGNORECASE = 1
    tangle_prop_regex = @/^\s*#\+property.*header-args.*:tangle\s*(\S.*)$/

    begin_src_regex = @/^\s*#\+begin_src/
    end_src_regex = @/^\s*#\+end_src/
    begin_src_tangle_or_eval_regex = @/^\s*#\+begin_src.*(:tangle|:eval)/

    begin_src_tangle_to_end_regex  = @/^\s*#\+begin_src.*:tangle\s*(\S.*)$/
    begin_src_eval_to_end_regex = @/^\s*#\+begin_src (sh|shell) .*:eval\s*(\S.*)$/

    double_quoted_value_regex = @/^"([^"]+)"/
    unquoted_value_regex = @/\s*(\S+)\s*.*/
    yes_or_no_regex = @/^['"]?(yes|t|no|nil)['"]?/
    no_or_nil_regex = @/^['"]?(no|nil)['"]?/
    org_escaped_asterix_regex = @/^(\s*,[*])/

    # (if (file-exists-p "~/.gitconfig") "no" "~/.gitconfig")
    elisp_file_exists_p_regex = @/\(if\s*\(file-exists-p\s*"([^"]+)"\)\s*"([^"]+)"\s*"([^"]+)"\s*\)/
    # (if (string-suffix-p "chip" hostname) "~/.i3/config" "no")
    elisp_string_suffix_p_regex = @/\(if\s*\(string-suffix-p\s*"([^"]+)"\s*([^)]+)\)\s*"([^"]+)"\s*"([^"]+)"\s*\)/
    # (if (eq system-type 'windows-nt) "yes" "no")
    elisp_system_type_regex = @/\(if\s*\(eq system-type\s*'([^)]+)\)\s*"([^"]+)"\s*"([^"]+)"\s*\)/

    uname_macos_regex = @/(darwin)/
    uname_msys_regex = @/(mingw|msys)/
    uname = ""
    uname_system_type = _get_uname_system_type()
    hostname = _get_hostname()

    # Src block condition attributes
    FILE_EXPRESSION = "file_expression"
    CONDITION_TYPE = "condition_type"
    # Expected values:
    #   raw_value, system_type, file_exists_p, string_suffix_p
    CONDITION_DATA1 = "condition_data1"
    CONDITION_DATA2 = "condition_data2"
    CONDITION_TRUE_CASE = "condition_true_case"
    CONDITION_FALSE_CASE = "condition_false_case"
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
        if (tangle_file_name() in tangled_files) {
            tangled_files[tangle_file_name()] = tangled_files[tangle_file_name()] "\n" current_line
        }
        # If this is the first line
        else {
            tangled_files[tangle_file_name()] = current_line
        }
        # print current_block_line_number,":",current_line
    }
}

# Check for start block line
#   Should come after in_block so the end_src line isn't printed
match($0, begin_src_tangle_or_eval_regex) {
    handle_tangle_or_eval_line($0)
}

function write_tangled_file(outfile, index_name, expanded_file_name) {
    if (expanded_file_name == "")
        return
    # If file name doesn't start with:
    #   (  -> isn't an elisp expression
    #   no -> should not be tangled
    if (match(expanded_file_name, /^\s*("?no"?|\()/, mg))
        return

    # remove any leading and trailing quotes
    sub(/^["]/, "", expanded_file_name)
    sub(/["]$/, "", expanded_file_name)
    # expand ~ to $HOME
    sub(/~/, ENVIRON["HOME"], expanded_file_name)

    if (expanded_file_name in final_tangled_file_list) {
        # append to the existing file
        print tangled_files[index_name] >> expanded_file_name
    }
    else {
        # first time we have tangled to this file
        final_tangled_file_list[expanded_file_name] = 1
        # print file being tangled
        print "  " expanded_file_name

        if (DRYRUN)
            return
        # always mkdir -p
        system("mkdir -v -p "dirname(expanded_file_name))
        # output contents string to the file all at once
        print tangled_files[index_name] > expanded_file_name
        # add tangled file path to outfile
        print expanded_file_name > outfile
    }
    # close files in ENDFILE rule
}

function run_script(shell_type, script_text) {
    if (DRYRUN)
        return
    if (shell_type == "sh") {
        print cli_debug("[EVAL]"), "sh"
        _DEBUG(shell_type)
        print script_text | "sh"
        close("sh")
    }
}

function get_destination_file_name(file_name) {
    destination_expression = ""

    if (block_conditions[file_name][CONDITION_TYPE] == "file_exists_p") {
        existing_file = block_conditions[file_name][CONDITION_DATA1]
        if (system("test -f " existing_file) == 0)
            destination_expression = block_conditions[file_name][CONDITION_TRUE_CASE]
        else
            destination_expression = block_conditions[file_name][CONDITION_FALSE_CASE]
    }

    # elisp string-suffix-p call
    else if (block_conditions[file_name][CONDITION_TYPE] == "string_suffix_p") {
        pattern = block_conditions[file_name][CONDITION_DATA1]
        variable = block_conditions[file_name][CONDITION_DATA2]
        if (variable == "hostname") {
            # check for suffix match
            p = pattern"$"
            if (hostname ~ p)
                destination_expression = block_conditions[file_name][CONDITION_TRUE_CASE]
            else
                destination_expression = block_conditions[file_name][CONDITION_FALSE_CASE]
        }
    }

    # elisp eq system-type call
    else if (block_conditions[file_name][CONDITION_TYPE] == "system_type") {
        system_type = block_conditions[file_name][CONDITION_DATA1]
        if (system_type == uname_system_type)
            destination_expression = block_conditions[file_name][CONDITION_TRUE_CASE]
        else
            destination_expression = block_conditions[file_name][CONDITION_FALSE_CASE]
    }

    # standard tangle file
    else {
        destination_expression = block_conditions[file_name][CONDITION_TRUE_CASE]
    }

    return destination_expression
}

ENDFILE {
    delete final_tangled_file_list

    outfile = ".cache/" FILENAME
    sub(/\.org$/, ".out", outfile)
    # Move existing .out file to .out.last

    if (!DRYRUN) {
        print "test -f "outfile" && cp "outfile" "outfile".last && rm "outfile | "sh"
        close("sh")
    }

    # Traverse array ordered by indices in ascending order compared as strings
    PROCINFO["sorted_in"] = "@ind_str_asc"

    # print_array_indexes(tangled_files)
    for (file_name in tangled_files) {
        _DEBUG(cli_warning(file_name))
        # run a script case
        if (match(file_name, /(BLOCK[0-9]+) eval-([0-9]+)-(sh|shell) (.*)$/, mg)) {
            shell_type = mg[3]
            # _DEBUG_ARRAY(block_conditions[file_name])
            if (get_destination_file_name(file_name) == "yes") {
                run_script(shell_type, tangled_files[file_name])
            }
        }
        # file tangle case
        else if (match(file_name, /^(BLOCK[0-9]+) (.*)$/, mg)) {
            _DEBUG_ARRAY(block_conditions[file_name])
            block_prefix = mg[1]
            expanded_file_name = mg[2]
            _DEBUG("[TANGLE] " expanded_file_name)

            write_tangled_file(outfile, file_name,
                               get_destination_file_name(file_name))
        }
    }

    if (!DRYRUN) {
        # close all final file names
        for (file_name in final_tangled_file_list)
            close(file_name)
        # close .out file
        close(outfile)
    }
}
