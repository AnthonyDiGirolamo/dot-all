#!/usr/bin/gawk -f

# Save max field width to the max_len array.
function check_length() {
    # Save this line
    lines[FNR] = $0
    # For each field
    for (i=1; i<=NF; i+=1) {
        l = length($(i))
        if (max_len[i] < l) {
            max_len[i] = l
        }
    }
}

function print_justified(line) {
    split(line, line_array)
    for (i=1; i<=length(line_array); i+=1) {
        # Column width of max field width + 1
        width = max_len[i] + 1
        # Format string that will Left justify the text
        fmt = "%-" width "s"
        printf fmt, line_array[i]
    }
    # Ending line break
    printf "\n"
}

{
    check_length()
}

END {
    for (i in lines) {
        print_justified(lines[i])
    }
}

