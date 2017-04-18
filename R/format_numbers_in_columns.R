# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Format numbers in each column of a dataframe
#
# data - data frame
# digits -
# the desired number of digits after the decimal point (format = "f") or significant digits (format = "g", = "e" or = "fg"). \cr
# Either one integer or a vector of inegers for each column.
#
# format - either one value or a vector of values for each column.
#       Each value Will be passed to `fun` separately.
#
# ... - to be passed to `fun`
# fun - formatting function to be applied. Default is `formatC`

#' Format numbers in columns
#'
#' Function formats only columns. Other classes are left intact.
#'
#' @param data A data-frame-like data.
#' @param digits (Either one integer or a vector of inegers for each column) a desired number of digits after the decimal point (if \code{format} = "f") or a number of significant digits (\code{format} = "g", = "e" or = "fg"). \cr
#' @param format either one value or a vector of values for each column.
#       Each value Will be passed to \code{fun} separately.
#' @param fun A function that does the formatting. Default is \code{\link[base]{formatC}}.
#' @param ... Other parameters to be passed to \code{fun}.
#'
#' @export
#'
#' @examples
#' library(BioStat)
#' DATA <- head(iris)
#'
#' # The same rounding for each column
#' format_numbers_in_columns(DATA, 2)
#'
#' # Different roundingfor each column
#' format_numbers_in_columns(DATA, c(2,2,3,3,NA))
#'
#' # Use NA to skip formatiing of the column
#' format_numbers_in_columns(DATA, c(4,NA,3,NA,NA))



format_numbers_in_columns <-
    function(data, digits = 3, format = "f", fun = formatC, ...) {

        n_col <- length(data)

        # Function for value recycling
        adjust_vector <- function(x) {
            len_x <- length(x)
            if (len_x == 1) {
                x <- rep_len(x, n_col)
            } else if (len_x != n_col) {
                stop("Length of `", substitute(x),
                     "` must be either 1 or ", n_col,".")
            }
            return(x)
        }

        # Apply the recycling
        digits <- adjust_vector(digits)
        format <- adjust_vector(format)

        # Apply the formatting
        for (i in seq_along(data)) {
            x <- data[[i]]
            if (!is.numeric(x)) next
            if (is.na(digits[i])) next
            data[[i]] <- fun(x, digits = digits[i], format = format[i], ...)
        }
        data
    }


#' Pretty formatting of a p-value
#'
#' @param x A p value.
#'
#' @return A string with p value in a pretty format.
#' @export
#'
#' @examples
#' pretty_p(0.005)
#'
#' pretty_p(0.0005)
#'
#' pretty_p(x = 0.052147)
#'
#'
pretty_p <- function(x) {
    if (x < 0.001) {
        "<0.001"
    } else if (x < 0.01) {
        "<0.01"
    } else {
        sprintf("%.2f", x)
    }
}

#' Format column of p-values in a data frame
#'
#' @param data A data frame
#' @param name A name of column with p-values
#' @param pretty_p (logical) should function \code{pretty_p} be applied.
#' @param ... (not used yet)
#'
#' @export
#'
#' @examples
#' rez <- test_normality(uptake ~ Type + Treatment, data = CO2)
#'
#' rez
#'
#' pretty_p_values(rez)
#'
pretty_p_values <- function(data,
                           name = "p.value",
                           pretty_p = TRUE,
                           ...)

    {
    x <- data[[name]]

        if (pretty_p == TRUE) {
            x <- sapply(x, pretty_p)
            data[[name]] <- x
        }

     data

}
