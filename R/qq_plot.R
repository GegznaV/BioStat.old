#' A qq-plot for multiple groups (with ggplot2)
#'
#' @param x Either a formula, a numeric vector or a name of a vector
#'          in \code{data}.
#'          If \code{x} is a formula (e.g. \code{variable ~ factor}), left-hand
#'          side provides variable to be summarized. Right-hand side and condition
#'          describe subsets. If the left-hand side is empty, right-hand side and
#'          condition are shifted over as a convenience.
#'
#' @param data A data frame that contains the variables mentioned in \code{x}.
#'
#' @param labels (not used yet).
#' @param sep (not used yet).
#'
#' @inheritParams qq_data
#' @inheritParams plot.qqdata
#' @inheritParams car::qqPlot
#' @inheritParams mosaic::maggregate
#' @inheritParams test_normality
#' @export
#' @seealso \code{\link[car]{qqPlot}} from \pkg{car} package,
#'          \code{\link[stats]{qqplot}} from \pkg{stats} package.
#' @examples
#' library(BioStat)
#' data(iris)
#'
#' # Formula (no groups):
#' qq_plot(~ Sepal.Length, data = iris)
#' qq_plot("Sepal.Length", data = iris)
#'
#' # Formula (several groups):
#' qq_plot(Sepal.Length ~ Species, data = iris)
#'
#' qq_plot(Sepal.Length ~ Species, data = iris, envelope = 0.90)
#'
#' # Formula (several groups in colors):
#' qq_plot(Sepal.Length ~ Species, data = iris, use_colors = TRUE)
#'
#' # Vectors (several groups):
#' qq_plot(iris$Sepal.Length, groups = iris$Species)
#'
#' # For one group:
#' qq_plot("Sepal.Length", data = iris)
#'
#' qq_plot(~Sepal.Length, data = iris)
#'
#' qq_plot(iris$Sepal.Length)
#'
#'
#' # Other examples
#'
#' qq_plot(~ weight, data = chickwts)
#'
#' qq_plot(weight ~ feed, data = chickwts)
#'
#' qq_plot(uptake ~ Type + Treatment, data = CO2)
#'
qq_plot <- function(x,
                    data = NULL,
                    distribution = "norm",
                    ...,
                    line = c("quartiles", "robust", "int=0,slope=1", "0,1", "none"),
                    envelope = 0.95,
                    method = c("mle-normal","trimmed-normal","moment-normal", "any"),
                    labels = NULL,
                    groups = NULL,
                    use_colors = FALSE,
                    scales = "free",
                    sep = "|")
{

    qqdata <-  qq_data(x = x,
                 distribution = distribution,
                 data = data,
                 ...,
                 envelope = envelope,
                 line = line,
                 labels = labels,
                 groups = groups,
                 method = method,
                 sep = "|")

    plot(qqdata,
         use_colors = use_colors,
         scales = scales,
         envelope = envelope,
         line = line,
         ...)

}

# =============================================================================

# group.CI <-
# function (x, data, ci = 0.95)
# {
#     return(group.UCL(x, data, FUN = CI, ci = ci))
# }
# # <environment: namespace:Rmisc>
#
#
#
# group.UCL <-
# function (x, data, FUN, ...)
# {
#     d <- aggregate(x, data, FUN = FUN, ...)
#     y <- colnames(d)[ncol(d)]
#     n <- as.data.frame(d[, y])
#     colnames(n) <- sapply(list("upper", "mean", "lower"), function(l) {
#         paste(y, l, sep = ".")
#     })
#     d[ncol(d)] <- NULL
#     return(cbind(d, n))
# }
# # <environment: namespace:Rmisc>
#
#
# aggregate(formula, data, FUN = FUN, ...)
#


# # x = weight, group = feed, data = weight
# # data1 <- with(chickwts, tapply(weight, feed, qq_data))
#
#      x <- chickwts$weight
# groups <- chickwts$feed
#
# qq_data_by_group <-
#     function(x, group = NULL, data = NULL, ...) {
#
#         x     <- getVarValues(x, data)
#         group <- getVarValues(group, data)
#
#         tapply(x, group, qq_data)  %>%
#             do.call(rbind, .) %>%
#             dplyr::mutate(ID = rownames(.))  %>%
#             tidyr::separate(ID, "ID", sep = "\\.\\d*$", extra = "drop")
#     }
#
# qq_data_by_group("weight", "feed", chickwts)

