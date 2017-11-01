#' Scale a Vector
#'
#' From every element in avector, subtract \code{center} and
#' divide by \code{scale}.
#'
#' @param x A numeric vector.
#' @param center Eiter a function that computes center of data
#'              (such as \code{mean}) or a simgle numeric value.
#' @param scale Eiter a function that computes variability of data
#'              (such as \code{sd}) or a simgle numeric value.
#'
#' @return The same object as \code{x} just with every element scaled
#' @export
#'
#' @examples
#'
#' x = 1:10
#' scale_vector(x)
#'
#' scale_vector(x, center = median, scale = IQR)
#'
#' scale_vector(x, center = 10, scale = 2)
#'
scale_vector <- function(x, center = mean, scale = sd) {
    # Preparation
    cener_ <-
        if (is.function(center)) {
            center(x)
        } else if (length(center) == 1) {
            center
        } else {
            stop("Incorrect value of 'center'")
        }

    scale_ <-
        if (is.function(scale)) {
            scale(x)
        } else if (length(scale) == 1) {
            scale
        } else {
            stop("Incorrect value of 'scale'")
        }

    # Transformation
    (x - cener_) / scale_
}