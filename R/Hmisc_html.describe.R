html.describe <- function(object,
                          size = 85,
                          tabular = TRUE,
                          greek = TRUE,
                          scroll = FALSE,
                          rows = 25,
                          cols = 100,
                          color = 'MidnightBlue',
                          ...) {

    at <- attributes(object)

    m <- markupSpecs$html
    center <- m$center
    bold   <- m$bold
    code   <- m$code
    br     <- m$br
    lspace <- m$lspace
    sskip  <- m$smallskip
    hrule  <- m$hrulethin
    fsize  <- m$size
    mnb    <- function(x) m$color(x,  color)

    R <- c(m$unicode, m$style()) ## define thinhr (and others not needed here)

    if (length(at$dimensions)) {
        R <- c(R,
               mnb(center(bold(
                   paste(htmlTranslate(at$descript),
                         sskip,
                         at$dimensions[2],
                         ' Variables',
                         lspace,
                         at$dimensions[1],
                         ' Observations'
                   )
               )))
        )

        if (length(at$naprint))
            R <- c(R, '', at$naprint)

        R <- c(R, hrule)

        vnames <- at$names
        i <- 0
        for (z in object) {
            i <- i + 1
            if (!length(z))
                next

            r <- html.describe.single(z,
                                      ## vname=vnames[i],
                                      tabular = tabular,
                                      greek = greek,
                                      size = size,
                                      color = color,
                                      ...
            )
            R <- c(R, r, hrule)
        }

        if (length(mv <- at$missing.vars)) {
            R <- c(R,
                   sskip,
                   'Variables with all observations missing:',
                   br,
                   sskip)
            mv <- paste(code(htmlTranslate(mv)), collapse = ', ')
            R <- c(R, mv)
        }

        if (scroll) R <- m$scroll( R,
                                   size = size,
                                   rows = rows,
                                   cols = cols,
                                   name = at$descript
        )
    } else {
        R <- c(R,
               html.describe.single( object,
                                     tabular = tabular,
                                     greek = greek,
                                     size = size,
                                     color = color,
                                     ...
               ))
    }

    htmltools::HTML(R)
}
# ------------------------------------------------------------------------------
