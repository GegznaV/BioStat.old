# ==============================================================================
print.describe <-   function(x, ...) {
    at <- attributes(x)
    if (length(at$dimensions)) {
        cat(at$descript,'\n\n',
            at$dimensions[2], ' Variables     ',
            at$dimensions[1], ' Observations\n')

        if (length(at$naprint))
            cat('\n', at$naprint, '\n')

        w <- paste(rep('-', .Options$width), collapse = '')
        cat(w, '\n', sep = '')

        for (z in x) {
            if (length(z) == 0) next

            print.describe.single(z, ...)
            cat(w, '\n', sep = '')
        }

        if (length(at$missing.vars)) {
            cat('\nVariables with all observations missing:\n\n')
            print(at$missing.vars, quote = FALSE)
        }

    } else {
        print.describe.single(x, ...)
    }

    invisible()
}

# ==============================================================================
print.describe.single <- function(x, ...) {
    wide <- .Options$width
    des  <- x$descript

    # if (length(x$units))
    #     des <- paste0(des, ' [', x$units, ']')
    #
    # if (length(x$format))
    #     fmt <- paste0(des, '  Format:', x$format)

    units_ <- if (length(x$units))  paste0(' [', x$units, ']')    else NULL
    fmt_   <- if (length(x$format)) paste0('  Format:', x$format) else NULL

    cat(des, units_, fmt_, '\n')

    print(x$counts, quote = FALSE)

    R <- formatdescribeSingle(x, lang = 'plain', ...)
    cat(R, sep = '\n')
    invisible()
}
# ==============================================================================
na.retain <- function(d) {
    d
}
# ==============================================================================
