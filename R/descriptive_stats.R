#' [!] Descriptive statistics
#'
#' @param x Variable to summarize
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
Q1 <- function(x, na.rm = TRUE,
               type = 7, names = FALSE, ...) {

    quantile(x, probs = 0.25, names = names, type = type, na.rm = na.rm, ...)
}

#' @rdname summary_funs
#' @export
Q3 <- function(x, na.rm = TRUE,
               type = 7,  names = FALSE, ...) {

    quantile(x, probs = 0.75, names = names, type = type, na.rm = na.rm, ...)
}

#' @rdname summary_funs
#' @export
n_missing <- function(x) {
    sum(is.na(x))
}

#' @rdname summary_funs
#' @export
n_ok <- function(x) {
    sum(!is.na(x))
}
