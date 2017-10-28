html.describe.single <- function(object,
                                 size = 85,
                                 tabular = TRUE,
                                 greek = TRUE,
                                 color = 'MidnightBlue',
                                 ...)
{

    m <- markupSpecs$html
    center  <- m$center
    bold    <- m$bold
    code    <- m$code
    br      <- m$br
    lspace  <- m$lspace
    sskip   <- m$smallskip
    fsize   <- m$size
    smaller <- m$smaller

    pngfile <- paste(tempdir(), 'needle1234567890a.png', sep = '/')

    oldw <- options('width')
    options(width = if (size < 90) 95 else 85)
    on.exit(options(oldw))

    wide <- if (size >= 90) 73 else if (size >= 75) 95 else 110

    z   <- htmlTranslate(object$descript, greek = greek)
    des <- if (!length(grep(':', z)))
        bold(z)
    else {
        ## Get text before : (variable name)
        sp <- strsplit(z, ' : ')[[1]]
        vnm <- sp[1]
        rem <- paste(sp[-1], collapse = ':')
        paste0(bold(vnm), ': ', rem)
    }

    if (length(object$units))
        des <- m$varlabel(des, htmlTranslate(object$units))

    if (length(object$format))
        des <- paste0(des,
                      lspace,
                      smaller(paste0('Format:',
                                     htmlTranslate(object$format))))

    Values <- object$values
    lco <-
        if (length(Values)) {
            length(Values$frequency)
        } else {
            0
        }

    if (lco > 2) {
        counts <- Values$frequency
        maxcounts <- max(counts)
        counts <- counts / maxcounts
        ## Scale distinct values to range from 1 : lco
        va <- Values$value
        if (!is.numeric(va)) {
            va <- 1:lco
        } else {
            rang <- range(va)
            va <- 1 + (lco - 1) * (va - rang[1]) / diff(rang)
        }
        w <- if (lco >= 50)
            150 / lco else 3
        des <- paste0(des,
                      m$rightAlign(tobase64image(
                          pngNeedle(
                              counts,
                              x = va,
                              w = w,
                              h = 13,
                              lwd = 2,
                              file = pngfile
                          )
                      )))
    }

    R <- des

    sz <- size
    if (tabular) {
        ml <- nchar(paste(object$counts, collapse = '  '))
        if (ml > 90) {
            tabular <- FALSE
        } else if (ml > 80) {
            sz <- round(0.875 * size)
        }
    }

    if (tabular) {
        d <- as.data.frame(as.list(object$counts))
        colnames(d) <- names(object$counts)
        tab <- html(d,
                    file = FALSE,
                    align = 'c',
                    align.header = 'c',
                    bold.header = FALSE,
                    col.header = color,
                    border = 0,
                    translate = TRUE,
                    size = sz
        )
        R <- c(R, tab)

    } else {
        R <- c(R, htmlVerbatim(object$counts, size = sz))
    }

    R <- c(R, formatdescribeSingle(object, lang = 'html', color = color, ...))
    R
}


