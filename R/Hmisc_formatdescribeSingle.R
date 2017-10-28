# ==============================================================================
## Function to format part of describe.single output after description & counts
## verb=1 means verbatim mode open
formatdescribeSingle <-
    function(x,
             condense = c('extremes', 'frequencies', 'both', 'none'),
             lang = c('plain', 'latex', 'html'),
             verb = 0,
             lspace = c(0, 0),
             size = 85,
             color = 'MidnightBlue',
             ...)
    {

        condense <- match.arg(condense)
        lang     <- match.arg(lang)
        wide     <- .Options$width
        specs    <- markupSpecs[[lang]]
        bv       <- function() {
            if (lang == 'latex' && !verb) {
                '\\begin{verbatim}'
            } else {
                character()
            }
        }
        vs       <- if (lang == 'latex' && lspace[2] != 0) {
            function() cat('\\vspace{', -lspace[2], 'ex}\n', sep = '')
        } else {
            function() {}
        }

        vbtm <- if (lang == 'html') {
            function(x, omit1b = FALSE, ...) {
                htmlVerbatim(x, size = size, omit1b = omit1b, ...)
            }
        } else {
            function(x, omit1b = NULL){
                capture.output(print(x, quote = FALSE, ...))}
        }

        R <- character(0)

        v <- x$values

        is.standard <- length(v) && is.list(v) && all(names(v) == c('value', 'frequency'))

        val.wide    <- length(v$value) && sum(nchar(as.character(v$value))) > 200
        val.few     <- length(v$value) && (length(v$value) <= 20) && (length(v$value) != 1)
        print.freq  <- is.standard && val.few && !val.wide
        print.ext   <- length(x$extremes) && !print.freq

        if (print.ext) {
            val  <- format(x$extremes)
            w    <- nchar(paste(val, collapse = ' '))
            R <- c(R, bv()); verb <- 1
            if (condense %in% c('extremes', 'both')) {
                if (lang == 'html') {
                    fsize <- specs$size
                    mnb <- function(x) specs$color(x, col=color)
                    spc <- specs$space
                    blo <- paste0(spc,  mnb('lowest:'))
                    bhi <- paste0(     mnb('highest:'))
                    if (w + 2 <= wide) {
                        low <- paste(blo, paste(val[1: 5], collapse = ' '))
                        hi  <- paste(bhi, paste(val[6:10], collapse = ' '))
                        R <- c(R, fsize(paste(low, ', ', hi), size))
                    } else {
                        low <- data.frame(name=blo, e1=val[1], e2=val[2], e3=val[3],
                                          e4=val[4], e5=val[5])
                        hi  <- data.frame(name=bhi, e1=val[6], e2=val[7], e3=val[8],
                                          e4=val[9], e5=val[10])
                        tab <- html(rbind(low, hi, make.row.names=FALSE),
                                    align='r',
                                    header=NULL, border=0, size=size, file=FALSE)
                        R <- c(R, tab)
                    }
                    # end lang='html'
                    #
                } else {# lang='plain' or 'latex'
                    low <- paste(' lowest:', paste(val[1:5],  collapse = ' '))
                    hi  <- paste('highest:', paste(val[6:10], collapse = ' '))
                    R <- c(R,
                           if (w + 2 <= wide) {
                               c('', paste0(low, ', ', hi))
                           } else {
                               c('', low, hi)
                           })
                }
                # end condense applicable to extremes
            }  else {
                R <- c(R, if (lang != 'html') '', vbtm(val))
            }
        }

        if (print.freq) {
            R <- c(R, bv()); verb <- 1
            val   <- v$value
            freq  <- v$frequency
            prop <- round(freq / sum(freq), 3)

            ## First try table output, if will fit in no more than 2 sets of 4 lines
            condensed <- TRUE
            if (condense %nin% c('frequencies', 'both')) {
                fval  <- if (is.numeric(val)) {
                    format(val)
                } else {
                    format(val, justify = 'right')
                }
                ffreq <- format(freq)
                fprop <- format(prop)
                lval  <- nchar(fval[1])
                lfreq <- nchar(ffreq[1])
                lprop <- nchar(fprop[1])

                m     <- max(lval, lfreq, lprop)
                ## Right justify entries in each row
                bl    <- '                                         '
                fval  <- paste0(substring(bl, 1, m - lval ), fval)
                ffreq <- paste0(substring(bl, 1, m - lfreq), ffreq)
                fprop <- paste0(substring(bl, 1, m - lprop), fprop)

                w <- rbind(Value = fval,
                           Frequency = ffreq,
                           Proportion = fprop)

                colnames(w) <- rep('', ncol(w))
                out <- capture.output(print(w, quote = FALSE))
                if (length(out) <= 8) {
                    R <- c(R, vbtm(w, omit1b = TRUE))
                    condensed <- FALSE
                }
            }   # end condense frequencies (or both)

            if (condensed) {
                fval  <- as.character(val)
                ffreq <- as.character(freq)
                fprop <- format(prop)
                lval  <- nchar(fval[1])
                lfreq <- nchar(ffreq[1])
                lprop <- nchar(fprop[1])
                w <- paste0(fval, ' (', ffreq, ', ', fprop, ')')
                w <- strwrap(paste(w, collapse = ', '), width = wide)
                R <- c(R, '', w)
            }
        } else if (length(v) && !is.standard) {
            R <- c(R, '', vbtm(v))
        }

        if (length(x$mChoice)) {
            R <- c(R, bv())
            verb <- 1
            R <- c(R, '', vbtm(x$mChoice, prlabel = FALSE))
        }

        if (lang == 'latex' && verb)
            R <- c(R, '\\end{verbatim}')

        R
    }
