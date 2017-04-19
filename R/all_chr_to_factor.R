#' Convert all character variables to factors
#'
#' @param data A data frame
#'
#' @return A data frame
#' @export
#'
#' @examples
#' library(BioStat)
#'
#' # Basic syntax
#' all_chr_to_factor(PlantGrowth)
#'
#' # Update the same object using operator from `magrittr` package
#' PlantGrowth %<>% all_chr_to_factor()
#'
#'
#' # Let's create a data frame
#' df <- data.frame(letters  = letters[1:5],
#'                  letters2 = LETTERS[1:5],
#'                  stringsAsFactors = FALSE)
#'
#' mapply(class, df) # show classes of columns
#'
#' # Let's convert all character variables to strings
#' df2 <- all_chr_to_factor(df)
#'
#' # Let's check the classes in each column
#' mapply(class, df2)
#'
#' # Works with tibbles too
#' tbl  <- as.tibble(df)
#' tbl2 <- all_chr_to_factor(tbl)
#' mapply(class, tbl2)

all_chr_to_factor <- function(data)
{
    col_classes <- mapply(class, data)
    names   <- colnames(data)

    for (i in seq_along(col_classes)) {
        if (is.character(col_classes[i])) {
            data[[i]] %<>% factor()
        }
    }
    # Output:
    data
}
