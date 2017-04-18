#' Convert all character variables to factors
#'
#' @param data A data frame
#'
#' @return A data frame
#' @export
#'
#' @examples
#' df <- data.frame(letters  = letters[1:5],
#'                  letters2 = LETTERS[1:5],
#'                  stringsAsFactors = FALSE)
#'
#' mapply(class, df) # show classes of columns
#'
#' df2 <- all_chr_to_factor(df)
#' mapply(class, df2)
#'
all_chr_to_factor <- function(data)
{
    for (i in seq_along(data)) {
        if (is.character(data[, i]) == TRUE) {
            data[, i]  %<>%  factor()
        }
    }
    data
}
