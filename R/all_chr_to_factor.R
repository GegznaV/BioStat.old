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
#' # Create a data frame
#' df <- data.frame(letters  = letters[1:5],
#'                  letters2 = LETTERS[1:5],
#'                  stringsAsFactors = FALSE)
#'
#' mapply(class, df) # show classes of columns
#'
#' # Convert all character variables to strings
#' df2 <- all_chr_to_factor(df)
#'
#' # Check the classes in each column
#' mapply(class, df2)

# # Works with tibbles too
# tbl  <- tibble::as.tibble(df)
# tbl2 <- all_chr_to_factor(tbl)
# mapply(class, tbl2)

all_chr_to_factor <- function(data)
{
    # Changes the class:
    #
    # data %<>%
    #     purrr::map_if(is.character, factor) %>%
    #     as.data.frame()
    #

    # Does not Change the class:
    col_classes <- purrr::map_chr(data, class)

    for (i in seq_along(col_classes)) {
        if (is.character(col_classes[i])) {
            data[[i]] %<>% factor()
        }
    }
    # Output:
    data
}