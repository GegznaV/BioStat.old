#' Get a slope and an intercept of a qqline
#'
#' @inheritParams stats::qqline
#'
#' @return list with a \code{$slope} and an \code{$intercept} for qqline
#' @export
#' @keywords internal
# @examples
qq_line_coeffs <-   function(y,
                              datax = FALSE,
                              distribution = qnorm,
                              probs = c(0.25, 0.75),
                              qtype = 7) {
    stopifnot(length(probs) == 2, is.function(distribution))
    y <-
        quantile(y,
                 probs,
                 names = FALSE,
                 type = qtype,
                 na.rm = TRUE)
    x <- distribution(probs)

    if (datax) {
        slope <- diff(x) / diff(y)
        intercept <- x[1L] - slope * y[1L]
    }
    else {
        slope <- diff(y) / diff(x)
        intercept <- y[1L] - slope * x[1L]
    }

    list(intercept, slope)
}
# =============================================================================