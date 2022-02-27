#!/usr/bin/gawk -f
# getopt.awk --- Do C library getopt(3) function in awk
#                Also supports long options.
# https://www.gnu.org/software/gawk/manual/html_node/Getopt-Function.html

# External variables:
#    Optind -- index in ARGV of first nonoption argument
#    Optarg -- string value of argument to current option
#    Opterr -- if nonzero, print our own diagnostic
#    Optopt -- current option letter

# Returns:
#    -1     at end of options
#    "?"    for unrecognized option
#    <s>    a string representing the current option

# Private Data:
#    _opti  -- index in multiflag option, e.g., -abc
function getopt(argc, argv, options, longopts,
                thisopt, i, j) {
    if (length(options) == 0 && length(longopts) == 0)
        return -1                # no options given

    if (argv[Optind] == "--") {  # all done
        Optind++
        _opti = 0
        return -1
    } else if (argv[Optind] !~ /^-[^:[:space:]]/) {
        _opti = 0
        return -1
    }
    if (argv[Optind] !~ /^--/) {        # if this is a short option
        if (_opti == 0)
            _opti = 2
        thisopt = substr(argv[Optind], _opti, 1)
        Optopt = thisopt
        i = index(options, thisopt)
        if (i == 0) {
            if (Opterr)
                printf("%c -- invalid option\n", thisopt) > "/dev/stderr"
            if (_opti >= length(argv[Optind])) {
                Optind++
                _opti = 0
            } else
                _opti++
            return "?"
        }
        if (substr(options, i + 1, 1) == ":") {
            # get option argument
            if (length(substr(argv[Optind], _opti + 1)) > 0)
                Optarg = substr(argv[Optind], _opti + 1)
            else
                Optarg = argv[++Optind]
            _opti = 0
        } else
            Optarg = ""
        if (_opti == 0 || _opti >= length(argv[Optind])) {
            Optind++
            _opti = 0
        } else
            _opti++
        return thisopt
    } else {
        j = index(argv[Optind], "=")
        if (j > 0)
            thisopt = substr(argv[Optind], 3, j - 3)
        else
            thisopt = substr(argv[Optind], 3)
        Optopt = thisopt
        i = match(longopts, "(^|,)" thisopt "($|[,:])")
        if (i == 0) {
            if (Opterr)
                 printf("%s -- invalid option\n", thisopt) > "/dev/stderr"
            Optind++
            return "?"
        }
        if (substr(longopts, i+1+length(thisopt), 1) == ":") {
            if (j > 0)
                Optarg = substr(argv[Optind], j + 1)
            else
                Optarg = argv[++Optind]
        } else
            Optarg = ""
        Optind++
        return thisopt
    }
}

BEGIN {
    Opterr = 1    # default is to diagnose
    Optind = 1    # skip ARGV[0]
    # _getopt_test = 1

    # test program
    if (_getopt_test) {
        _myshortopts = "ab:cd"
        _mylongopts = "longa,longb:,otherc,otherd"

        # Debug print
        # for (i = 0; i < ARGC; i++)
        #     printf("\tARGV[%d] = <%s>\n", i, ARGV[i])

        while ((_go_c = getopt(ARGC, ARGV, _myshortopts, _mylongopts)) != -1)
            printf("c = <%s>, Optarg = <%s>\n", _go_c, Optarg)

        # Erase handled args
        for (i = 1; i < Optind; i++)
            ARGV[i] = ""
        # Debug print
        # for (i = 1; i < ARGC; i++)
        #     printf("\tARGV[%d] = <%s>\n", i, ARGV[i])

        printf("non-option arguments:\n")
        for (; Optind < ARGC; Optind++)
            printf("\tARGV[%d] = <%s>\n", Optind, ARGV[Optind])


    }
}
