# head_tail -------------------------------------------------------------------

#' Show several first and last rows of a data frame
#'
#' @param x A data frame.
#' @param n (integer) Number of top and bottom rows to display.
#' @param top (integer) Number of top rows to display.
#' @param bottom (integer) Number of bottom rows to display.
#' @param sep (character) Separator between displayed top and bottom lines.
#'
#' @return A truncated data frame (which is intended to be printed) with all variables converted to strings.
#' @export
#'
#' @examples
#' data(swiss)
#' head_tail(swiss)
head_tail <- function(x,
                      n = 4,
                      top = n,
                      bottom = n,
                      sep = "...") {
    x <- dplyr::mutate_all(as.data.frame(x), as.character)
    h <- head(x, top)
    t <- tail(x, bottom)

    dots  <- rep(sep, ncol(x))
    space <- rep(" ", ncol(x))
    rbind(h, `...` = dots, t, `  ` = space)
}


#' Descriptive statistics
#'
#' @param x Variable to summarize
#' @param ... futher arguments to methods
#' @inheritParams stats::quantile
#'
#' @return Summary statistic(s).
#' @export
#' @aliases descriptives Q1 Q2 n_missing n_ok
#'
Q1 <- function(x, na.rm = TRUE,
               type = 7, names = FALSE, ...) {

    quantile(x, probs = 0.25, names = names, type = type, na.rm = na.rm, ...)
}

#' @export
Q3 <- function(x, na.rm = TRUE,
               type = 7,  names = FALSE, ...) {

    quantile(x, probs = 0.75, names = names, type = type, na.rm = na.rm, ...)
}

#' @export
n_missing <- function(x) {
    sum(is.na(x))
}

#' @export
n_ok <- function(x) {
    sum(!is.na(x))
}
#' @export
n_incl <- function(x) {
    sum(!is.na(x))
}