#' Get a Slope and an Intercept of a QQ-line
#'
#' @param y (numeric) A numeric vector.
#'
#' @inheritParams stats::qqline
#' @inheritParams stats::quantile
#'
#' @return A vector with a \code{$slope} and an \code{$intercept} for qqline
#' @export
#' @keywords internal
#' @examples
#'
#' set.seed(254)
#' x <- rnorm(50)
#'
#' qq_line_coeffs(x)
#'
qq_line_coeffs <- function(y,
                           datax = FALSE,
                           distribution = qnorm,
                           probs = c(0.25, 0.75),
                           qtype = 7,
                           na.rm = TRUE) {
    stopifnot(length(probs) == 2, is.function(distribution))
    y <-
        stats::quantile(y,
                 probs,
                 names = FALSE,
                 type = qtype,
                 na.rm = na.rm)
    x <- distribution(probs)

    if (datax) {
        slope <- diff(x) / diff(y)
        intercept <- x[1L] - slope * y[1L]
    }
    else {
        slope <- diff(y) / diff(x)
        intercept <- y[1L] - slope * x[1L]
    }
    # Output
    c(intercept = intercept, slope = slope)
}
# =============================================================================