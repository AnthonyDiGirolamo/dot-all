#!/usr/bin/gawk -f
# rewind() from https://www.gnu.org/software/gawk/manual/gawk.pdf
function rewind(i) {
    # Shift remaining arguments up
    for (i = ARGC; i > ARGIND; i--)
        ARGV[i] = ARGV[i-1]
    # Make sure gawk knows to keep going
    ARGC++
    # Make current file next to get done
    ARGV[ARGIND+1] = FILENAME
    # Do it
    nextfile
}

# Save max field width to the max_len array.
function check_length() {
    # For each field
    for (i=1; i<=NF; i+=1) {
        l = length($(i))
        if (max_len[i] < l) {
            max_len[i] = l
        }
    }
}

preprocessed == 0 {
    # If not processed read every line and find the max field widths.
    check_length()
    while (getline > 0) {
        check_length()
    }
    # Flag this block as done
    preprocessed = 1
    # Rewind or re-process the first file.
    rewind()
}

preprocessed == 1 {
    for (i=1; i<=NF; i+=1) {
        # Column width of max field width + 1
        width = max_len[i] + 1
        # Format string that will Left justify the text
        fmt = "%-" width "s"
        printf fmt, $(i)
    }
    # Ending line break
    printf "\n"
}
