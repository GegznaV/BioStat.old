
#' A qq-plot for multiple groups (with ggplot2)
#'
#' @param ...
#' @inheritParams qq_data
#' @inheritParams car::qqPlot
#' @inheritParams plot.qqdata
#' @export
#'
#' @examples
#' library(BioStat)
#' data(iris)
#'
#' # Formula (several groups):
#' qq_plot(Sepal.Length ~ Species, data = iris, envelope = 0.95)
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
qq_plot <- function(x,
                    data = NULL,
                    distribution = "norm",
                    ...,
                    envelope = 0.95,
                    line = c("quartiles", "robust", "int=0,slope=1"),
                    labels = NULL,
                    groups = NULL,
                    method = if (distribution == "norm") {"normal"} else {"any"},
                    use_colors = FALSE,
                    scales = "free")
{

    qqdata <-  qq_data(x = x,
                 distribution = distribution,
                 data = data,
                 ...,
                 envelope = envelope,
                 line = line,
                 labels = labels,
                 groups = groups,
                 method = method)

    plot(qqdata, use_colors = use_colors, scales = scales)

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
#
#
#
#
