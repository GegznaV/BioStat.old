#' Trim certain percentage of the most extreme values
#'
#' Trim certain percentage of the most extreme values in a numeric vector.
#'
#' @param x A numeric vector
#' @param trim (a number between 0 and 1) A proportion of values to be removed.
#' @param na.rm  (logical) if \code{TRUE}, any \code{NA} and \code{NaN}'s are
#'               removed from \code{x} before quantiles are computed.
#' @param ... further argument to \code{\link[stats]{quantile}}.
#'
#' @return A trimmed vector.
#' @export
#'
#' @examples
#' 1:10
#' # [1] 1 2 3 4 5 6 7 8 9 10
#'
#' trim(1:10)
#' # [1] 2 3 4 5 6 7 8 9
#'
#' trim(1:10, trim = 0.4)
#' # [1] 3 4 5 6 7 8
#'
trim <- function(x, trim = 0.1, na.rm = FALSE, ...){
    p <- quantile(x, probs = c(trim/2, 1 - trim/2), na.rm = na.rm, ...)
    # x[x > p[1] & x < p[2]]
    dplyr::between(x, p[1], p[2])
}