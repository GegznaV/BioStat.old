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
#' @param x A p-value (numeric or coercible to numeric).
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
#' # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Prettify single p-values
#'
#' prettify_p_value(0.005)
#' prettify_p_value(0.0005)
#' prettify_p_value(0.052147)
#' prettify_p_value("0.0022")
#' prettify_p_value("0.052")
#'
prettify_p_value <- function(x) {
    if (length(x) != 1)          {stop("The langth of `x` must be 1.") }

    # x %<>% readr::parse_number(x)
    x %<>% as.character() %>% as.numeric()

    if (!dplyr::between(x, 0, 1)) {stop("`x` must be between 0 and 1.")}

    if (x < 0.001) {
        "<0.001"
    } else if (x < 0.01) {
        "<0.01 "
    } else {
        sprintf(" %.2f ", x)
    }
}

#' @rdname prettify_p_value
#' @export
prettify_p_column <- function(data,
                              colname  = "p.value",
                              prettify = TRUE,
                              ...)
{
    if (prettify == TRUE) {
        data[[colname]] %<>% purrr::map_chr(prettify_p_value)
    }
    data
}