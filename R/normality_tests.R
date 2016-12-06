
# setGeneric("shapiro.test")
# methods("shapiro.test")

test_ <- function (x, ..., data = NULL,
                   groups = NULL,
                   FUN = stats::shapiro.test)
    # na.rm = getOption("na.rm", FALSE)
{
    if (lazyeval::is_formula(x)) {
        if (is.null(data))
            data <- lazyeval::f_env(x)

        formula <- mosaic_formula_q(x, groups = groups, max.slots = 3)

        return(maggregate(formula,
                          data = data,
                          FUN = FUN,
                          ...,
                          .multiple = TRUE))
    }

    FUN(x, ...)
}

# =============================================================================
#' Normality tests with formula interface
#'
#' @param x
#' @param ...
#' @inheritParams mosaic::maggregate
#'
#' @return
#' @export
#' @importFrom nortest pearson.test, sf.test, ad.test, lillie.test, cvm.test
#' @examples
#' library(BioStat)
#' data(CO2)
#' shapiro.test(uptake ~ Type + Treatment, data = CO2)
#'
shapiro.test <- function(x, ..., data = NULL, groups = NULL) {
    test_(x, ..., data = data, groups = groups, FUN = stats::shapiro.test)
}

# =============================================================================

#' @rdname shapiro.test
#' @export
lillie.test <- function(x, ..., data = NULL, groups = NULL) {
    test_(x, ..., data = data, groups = groups, FUN = nortest::lillie.test)
}

# =============================================================================
#' @rdname shapiro.test
#' @export
ad.test <- function(x, ..., data = NULL, groups = NULL) {
    test_(x, ..., data = data, groups = groups, FUN = nortest::ad.test)
}

# =============================================================================
#' @rdname shapiro.test
#' @export
cvm.test <- function(x, ..., data = NULL, groups = NULL) {
    test_(x, ..., data = data, groups = groups, FUN = nortest::cvm.test)
}

# =============================================================================
#' @rdname shapiro.test
#' @export
sf.test <- function(x, ..., data = NULL, groups = NULL) {
    test_(x, ..., data = data, groups = groups, FUN = nortest::sf.test)
}
# =============================================================================
#' @rdname shapiro.test
#' @export
pearson.test <- function(x, n.classes = ceiling(2 * (n^(2/5))),
                         adjust = TRUE,
                         data = NULL,
                         groups = NULL) {
    test_(x,
          n.classes = n.classes,
          adjust = adjust,
          data = data,
          groups = groups,
          FUN = nortest::pearson.test)
}
# =============================================================================
