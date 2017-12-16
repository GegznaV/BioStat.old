#' [!] Descriptive statistics
#'
#' @param y Variable to summarize
#' @param ... futher arguments to methods
#' @inheritParams stats::quantile
#'
#' @return Summary statistic(s).
#' @export
#' @keywords utilities
#'
#' @aliases summary_funs Q1 Q2 n_missing n_ok
#'
#' @name summary_funs
Q1 <- function(y, na.rm = TRUE,
               type = 7, names = FALSE, ...) {

    quantile(y, probs = 0.25, names = names, type = type, na.rm = na.rm, ...)
}

#' @rdname summary_funs
#' @export
Q3 <- function(y, na.rm = TRUE,
               type = 7,  names = FALSE, ...) {

    quantile(y, probs = 0.75, names = names, type = type, na.rm = na.rm, ...)
}

#' @rdname summary_funs
#' @export
n_missing <- function(y) {
    sum(is.na(y))
}

#' @rdname summary_funs
#' @export
n_ok <- function(y) {
    sum(!is.na(y))
}
