# ====================================================================
#' Format p-values
#'
#' Values below 0.001 are printed as \code{"<0.001"},
#' between [0.001 and 0.01) as \code{"<0.01"},
#' all others are rounded to two decimal places. \cr
#'
#' \code{prettify_p_column} formats a column of p-values in a data frame.\cr
#'
#' \code{prettify_p_column} formats a single value.\cr
#'
#' @param p A p-value (numeric or coercible to numeric).
#' @param data A data frame.
#' @param colname (string) A name of column with p-values.
#' @param prettify (logical) should function \code{prettify_p_value}
#' be applied.
#' @param ... (not used yet)
#'
#' @export
#' @examples
#' library(BioStat)
#'
#' # Make a dataframe with p-values
#' rez <- test_normality(uptake ~ Type,
#'                       data = CO2)
#' rez
#'
#' # Shapiro-Wilk normality test
#' #
#' #          Type statistic      p.value
#' # 1      Quebec 0.8598812 0.0001111111
#' # 2 Mississippi 0.9363277 0.0213016282
#'
#' # Do prettification:
#' prettify_p_column(rez)
#'
#' # Shapiro-Wilk normality test
#' #
#' #          Type statistic p.value
#' # 1      Quebec 0.8598812  <0.001
#' # 2 Mississippi 0.9363277   0.02
#'
#' prettify_p_column(rez, rm_zero = TRUE)
#'
#' # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Prettify single p-values
#'
#' prettify_p_value(0.005)
#' prettify_p_value(0.0005)
#' prettify_p_value(0.052147)
#' prettify_p_value("0.0022")
#' prettify_p_value("0.052")
#'
#'
#' add_signif_stars(0.005)
#' add_signif_stars(0.0005)
#' add_signif_stars(0.052147)
#'
prettify_p_value <- function(p) {
    if (length(p) != 1)          {stop("The langth of `p` must be 1.") }

    # p %<>% readr::parse_number(p)
    p %<>% as.character() %>% as.numeric()

    if (!dplyr::between(p, 0, 1)) {stop("`p` must be between 0 and 1.")}

    if (p < 0.001) {
        "<0.001"
    } else if (p < 0.01) {
        "<0.01 "
    } else {
        sprintf(" %.2f ", p)
    }
}

#' @rdname prettify_p_value
#' @export
#' @param (logical) If \code{TRUE}, leading zero of a number is removed.
prettify_p_column <- function(data,
                              colname  = c("p.value", "p.adjust"),
                              prettify = TRUE,
                              rm_zero = FALSE,
                              ...)
{
    data_colnames <- names(data)
    colname <- data_colnames[data_colnames %in% colname]

    for (colname_i in colname) {
        if (prettify == TRUE) {
            data[[colname_i]] %<>% purrr::map_chr(prettify_p_value)
        }

        if (rm_zero == TRUE) {
            data[[colname_i]] %<>% purrr::map_chr(BioStat::rm_zero)
        }
    }
    data
}


#' @rdname prettify_p_value
#' @export
add_signif_stars <- function(p) {

        Signif <- symnum(
            p,
            corr = FALSE,
            na = FALSE,
            cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
            symbols = c("***", "**", "*", ".", " ")
        )
        paste(p, format(Signif))
}

###

#' Function to remove zero at the beginning of a number
#'
#' @param str a string or convertible to string
#'
#' @return A value without leading zero
#' @export
#' @keywords internal
#' @examples
#'
#' rm_zero(0.02)
#'
rm_zero <- function(str) {
    gsub("0\\.", ".", str)
}