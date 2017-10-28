# ==============================================================================
dataDensityString <- function(x, nint = 30) {
    x <- as.numeric(x)
    x <- x[!is.na(x)]
    if (length(x) < 2)
        return('')
    r <- range(x)
    x <- floor(nint * (x - r[1]) / (r[2] - r[1]))
    x <- pmin(tabulate(x), 37)
    paste0(format(r[1]), ' <',
           paste(substring(' 1234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ',
                           x + 1,
                           x + 1
           ), collapse = ''),
           '> ',
           format(r[2]))
}