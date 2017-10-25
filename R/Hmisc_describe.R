# ==============================================================================
#' describe
#'
#' From "Hmisc"
#'
#' @param x object to make a statistical summary for
#' @param ... other parameters
#'
#' @export
#' stat_summary
#' @export
describe_data <- function(x, ...) {
    UseMethod("describe_data")
}
#
# ------------------------------------------------------------------------------
#' @export
describe_datadefault <- function(x, descript, ...) {
    if (missing(descript)) {
        descript <- deparse(substitute(x))
    }

    if (is.matrix(x)) {
        describe_datamatrix(x, descript, ...)
    } else {
        describe_datavector(x, descript, ...)
    }
}
# ------------------------------------------------------------------------------
describe_datamatrix <- function(x,
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
        z <- describe_datavector(x[, i],
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
#' @export
describe_datadata.frame <- function(x,
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
            describe_datamatrix(xx,
                            nam[i],
                            exclude.missing = exclude.missing,
                            digits = digits,
                            ...)
        } else {
            describe_datavector(xx,
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
#' @export
describe_dataformula <- function(x,
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

    Z <- describe_datadata.frame(mf,
                                 descript,
                                 digits = digits,
                                 weights = weights,
                                 ...)

    if (length(z <- attr(mf, "na.action")))
        attr(Z, 'naprint') <- naprint(z)

    Z
}


