# ==============================================================================
describe <- function(x, ...) {
    UseMethod("describe")
}
# ------------------------------------------------------------------------------
describe.default <- function(x, descript, ...) {
    if (missing(descript)) {
        descript <- deparse(substitute(x))
    }

    if (is.matrix(x)) {
        describe.matrix(x, descript, ...)
    } else {
        describe.vector(x, descript, ...)
    }
}
# ------------------------------------------------------------------------------
describe.vector <- function(x,
                            descript,
                            exclude.missing = TRUE,
                            digits = 4,
                            listunique = 0,
                            listnchar = 12,
                            weights = NULL,
                            normwt = FALSE,
                            minlength = NULL,
                            ...)

    {
        oldopt <- options('digits')
        options(digits = digits)
        on.exit(options(oldopt))

        weighted <- length(weights) > 0
        if (!weighted)
            weights <- rep(1, length(x))

        special.codes <- attr(x, "special.miss")$codes
        labx <- attr(x, "label")

        if (missing(descript))
            descript <- as.character(sys.call())[2]

        if (length(labx) && labx != descript)
            descript <- paste(descript, ":", labx)

        un <- attr(x, "units")
        if (length(un) && un == '')
            un <- NULL

        fmt <- attr(x, 'format')
        if (length(fmt) && (is.function(fmt) || fmt == ''))
            fmt <- NULL

        if (length(fmt) > 1)
            fmt <- paste(as.character(fmt[[1]]), as.character(fmt[[2]]))

        present <- if (all(is.na(x))) {
            rep(FALSE, length(x))
        } else if (is.character(x)) {
            x != "" & x != " " & !is.na(x)
        } else{
            !is.na(x)
        }

        present <- present & !is.na(weights)

        if (length(weights) != length(x))
            stop('length of weights must equal length of x')

        if (normwt) {
            weights <- sum(present) * weights / sum(weights[present])
            n <- sum(present)
        } else
            n <- sum(weights[present])

        if (exclude.missing && n == 0)
            return(structure(list(), class = "describe"))

        missing <- sum(weights[!present], na.rm = TRUE)
        atx <- attributes(x)
        atx$names <- atx$dimnames <- atx$dim <- atx$special.miss <- NULL

        atx$class <- atx$class[atx$class != 'special.miss']

        isdot <- testDateTime(x, 'either') # is date or time var
        isdat <- testDateTime(x, 'both')   # is date and time combo var

        x <- x[present, drop = FALSE]
        x.unique <- sort(unique(x))
        weights <- weights[present]

        n.unique <- length(x.unique)
        attributes(x) <- attributes(x.unique) <- atx

        isnum <- (is.numeric(x) || isdat) && !is.factor(x)
        timeUsed <- isdat && testDateTime(x.unique, 'timeVaries')

        z <- list(descript = descript,
                  units = un,
                  format = fmt)

        counts <- c(n, missing)
        lab <- c("n", "missing")

        if (length(special.codes)) {
            tabsc <- table(special.codes)
            counts <- c(counts, tabsc)
            lab <- c(lab, names(tabsc))
        }

        if (length(atx$imputed)) {
            counts <- c(counts, length(atx$imputed))
            lab <- c(lab, "imputed")
        }

        if (length(pd <- atx$partial.date)) {
            if ((nn <- length(pd$month)) > 0) {
                counts <- c(counts, nn)
                lab <- c(lab, "missing month")
            }

            if ((nn <- length(pd$day)) > 0) {
                counts <- c(counts, nn)
                lab <- c(lab, "missing day")
            }

            if ((nn <- length(pd$both)) > 0) {
                counts <- c(counts, nn)
                lab <- c(lab, "missing month,day")
            }
        }

        if (length(atx$substi.source)) {
            tabss <- table(atx$substi.source)
            counts <- c(counts, tabss)
            lab <- c(lab, names(tabss))
        }

        counts <- c(counts, n.unique)
        lab <- c(lab, "distinct")

        if (isnum) {
            xnum <- unclass(x)
            if (n.unique < 2)
                reff <- 0
            else {
                fp <- wtd.table(xnum,
                                weights,
                                normwt = FALSE,
                                na.rm = FALSE,
                                type = 'table') / sum(weights)
                reff   <- (1 - sum(fp ^ 3)) / (1 - 1 / n / n)
            }
            counts <- c(counts, round(reff, 3))
            lab    <- c(lab, 'Info')
        }

        x.binary <-
            n.unique == 2 && isnum && x.unique[1] == 0 && x.unique[2] == 1
        if (x.binary) {
            counts <- c(counts, sum(weights[x == 1]))
            lab <- c(lab, "Sum")
        }

        if (isnum) {
            if (isdot) {
                dd <- sum(weights * xnum)  / sum(weights)
                fval <- formatDateTime(dd, atx, !timeUsed)
                counts <- c(counts, fval)
            } else
                counts <- c(counts, format(sum(weights * x) / sum(weights), ...))

            lab <- c(lab, "Mean")
            if (!weighted) {
                gmd <- GiniMd(xnum)
                counts <-
                    c(counts,
                      if (isdot)
                          formatDateTime(gmd, atx, !timeUsed)
                      else
                          format(gmd, ...))
                lab <- c(lab, "Gmd")
            }
        } else if (n.unique == 1) {
            counts <- c(counts, format(x.unique))
            lab <- c(lab, "value")
        }

        if (n.unique >= 10 & isnum) {
            q <- if (any(weights != 1)) {
                wtd.quantile(xnum,
                             weights,
                             normwt = FALSE,
                             na.rm = FALSE,
                             probs = c(.05, .1, .25, .5, .75, .90, .95))
            } else {
                quantile(xnum, c(.05, .1, .25, .5, .75, .90, .95), na.rm = FALSE)
            }

            ## Only reason to call quantile is that the two functions can give
            ## different results if there are ties, and users are used to quantile()
            fval <- if (isdot) {
                formatDateTime(q, atx, !timeUsed)
            } else {
                format(q, ...)
            }

            counts <- c(counts, fval)
            lab <- c(lab, ".05", ".10", ".25", ".50", ".75", ".90", ".95")
        }
        names(counts) <- lab
        z$counts <- counts

        tableIgnoreCaseWhiteSpace <- function(x) {
            x <- gsub('\r', ' ', x)
            x <- gsub('^[[:space:]]+', '', gsub('[[:space:]]+$', '', x))
            x <- gsub('[[:space:]]+', ' ', x)
            y <- tolower(x)
            f <- table(y)
            names(f) <- x[match(names(f), y)]
            f
        }

        values <- NULL
        if (!x.binary) {
            if (inherits(x, 'mChoice')) {
                z$mChoice <- summary(x, minlength = minlength)
            } else if (n.unique <= listunique &&
                       !isnum &&
                       !is.factor(x) && bmax(nchar(x)) > listnchar) {
                values <- tableIgnoreCaseWhiteSpace(x)
            } else if (isnum || n.unique <= 100) {
                if (isnum) {
                    if (n.unique >= 100 ||
                        min(diff(sort(unique(xnum)))) < diff(range(xnum)) / 500) {
                        pret <- pretty(xnum, if (n.unique >= 100)
                            100
                            else
                                500)
                        dist <- pret[2] - pret[1]
                        r    <- range(pret)
                        xnum <-
                            r[1] + dist * round((xnum - r[1]) / dist)
                    }
                }
                values <-
                    wtd.table(if (isnum) xnum else if (isdat) format(x) else x,
                              weights,
                              normwt = FALSE,
                              na.rm = FALSE)

                values <- list(value = values$x,
                               frequency = unname(values$sum.of.weights))
            }
            z$values <- values

            if (n.unique >= 5) {
                loandhi <- x.unique[c(1:5, (n.unique - 4):n.unique)]
                extremes <-
                    if (isdot &&
                        (class(loandhi) %nin% 'timeDate')) {
                        formatDateTime(unclass(loandhi),
                                       at = atx,
                                       roundDay = !timeUsed)
                    } else if (isnum) {
                        loandhi
                    } else {
                        format(format(loandhi), ...)
                    }
                names(extremes) <-
                    c("L1", "L2", "L3", "L4", "L5", "H5", "H4", "H3", "H2", "H1")
                z$extremes <- extremes
            }
        }
        structure(z, class = "describe")
}

# ------------------------------------------------------------------------------
describe.matrix <- function(x,
                            descript,
                            exclude.missing = TRUE,
                            digits = 4,
                            ...)
{
    if (missing(descript))
        descript <- as.character(sys.call())[2]

    nam <- dimnames(x)[[2]]

    if (length(nam) == 0)
        stop('matrix does not have column names')

    Z <- vector('list', length(nam))
    names(Z) <- nam

    d <- dim(x)
    missing.vars <- NULL
    for (i in 1:ncol(x)) {
        z <- describe.vector(x[, i],
                             nam[i],
                             exclude.missing = exclude.missing,
                             digits = digits,
                             ...)  #13Mar99
        Z[[i]] <- z
        if (exclude.missing && length(z) == 0)
            missing.vars <- c(missing.vars, nam[i])
    }

    attr(Z, 'descript') <- descript
    attr(Z, 'dimensions') <- d
    attr(Z, 'missing.vars') <- missing.vars
    structure(Z, class = "describe")
}

# ------------------------------------------------------------------------------
describe.data.frame <- function(x,
                                descript,
                                exclude.missing = TRUE,
                                digits = 4,
                                ...)
{
    if (missing(descript))
        descript <- as.character(sys.call())[2]

    nam <- names(x)
    Z <- list()
    nams <- character(0)

    i <- 0
    missing.vars <- NULL
    for (xx in x) {
        mat <- is.matrix(xx)
        i <- i + 1
        z <- if (mat) {
            describe.matrix(xx,
                            nam[i],
                            exclude.missing = exclude.missing,
                            digits = digits,
                            ...)
        } else {
            describe.vector(xx,
                            nam[i],
                            exclude.missing = exclude.missing,
                            digits = digits,
                            ...)
        }

        all.missing <- length(z) == 0
        if (exclude.missing && all.missing) {
            missing.vars <- c(missing.vars, nam[i])
        } else {
               Z <- c(Z,    if (mat)       z  else list(z))
            nams <- c(nams, if (mat) names(z) else nam[i])
        }
    }
    names(Z) <- nams

    attr(Z, 'descript') <- descript
    attr(Z, 'dimensions') <- dim(x)
    attr(Z, 'missing.vars') <- missing.vars
    structure(Z, class = "describe")
}

# ------------------------------------------------------------------------------
describe.formula <- function(x,
                             descript,
                             data,
                             subset,
                             na.action,
                             digits = 4,
                             weights,
                             ...)
{
    mf <- match.call(expand.dots = FALSE)
    mf$formula <- x
    mf$x <- mf$descript <- mf$file <- mf$append <- mf$... <- mf$digits <- NULL

    if (missing(na.action))
        mf$na.action <- na.retain

    mf[[1]] <- as.name("model.frame")
    mf <- eval(mf, sys.parent())
    weights <- model.extract(mf, weights)

    if (missing(descript)) {
        ter <- attr(mf, "terms")
        d <- as.character(x)
        if (attr(ter, "response") == 1) {
            d <- c(d[2], d[1], d[-(1:2)])
        } else {
            d <- d[-1]
        }
        d <- paste(d, collapse = " ")
        descript <- d
    }

    Z <- describe.data.frame(mf, descript, digits = digits, weights = weights, ...)

    if (length(z <- attr(mf, "na.action")))
        attr(Z, 'naprint') <- naprint(z)

    Z
}


