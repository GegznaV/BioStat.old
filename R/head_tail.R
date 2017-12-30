# head_tail -------------------------------------------------------------------

#' [!] Show several first and last rows of a data frame
#'
#' @param obj A data frame.
#' @param n (integer) Number of top and bottom rows to display.
#' @param top (integer) Number of top rows to display.
#' @param bottom (integer) Number of bottom rows to display.
#' @param sep (character) Separator between displayed top and bottom lines.
#'
#' @return A truncated data frame (which is intended to be printed) with all
#'         variables converted to strings.
#' @export
#'
#' @keywords utilities
#'
#' @examples
#' data(swiss)
#' head_tail(swiss)
head_tail <- function(obj,
                      n = 4,
                      top = n,
                      bottom = n,
                      sep = "...") {
    obj <- dplyr::mutate_all(as.data.frame(obj), as.character)
    h <- head(obj, top)
    t <- tail(obj, bottom)

    dots  <- rep(sep, ncol(obj))
    space <- rep(" ", ncol(obj))
    rbind(h, `...` = dots, t, `  ` = space)
}
