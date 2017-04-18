# =============================================================================
#' Plot qqdata object as qqplot
#'
#' @param x qqdata object
#' @param ... other parameters
#' @param scales ("free"|"free_x"|"free_y"|"fixed") a parmeter to be passed to \code{\link[ggplot2]{facet_wrap}}.
#' @param use_colors (logical) use colors for multiple groups
#'
#' @export
#' @import ggplot2 magrittr
#'
#' @examples
#' library(BioStat)
#' data(chickwts, package = "datasets")
#'
#' # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Input as formula + data:
#'
#' QQ_groups <- qq_data(weight ~ feed, data = chickwts, method = "normal")
#' plot(QQ_groups)
#'
#' QQ_groups <- qq_data(weight ~ feed, data = chickwts, method = "any")
#' plot(QQ_groups)
#'
#' plot(QQ_groups, scales = "fixed")
#' plot(QQ_groups, use_colors = TRUE)
#'
#'
#' QQ_single <- qq_data( ~ weight, data = chickwts)
#' plot(QQ_single)
#'
#' class(QQ_single)
#'
#'# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'
#' data(CO2, package = "datasets")
#'
#'  QQ_CO2 <- qq_data(uptake ~ Type + Treatment, data = CO2)
#'  plot(QQ_CO2)
#'
#'  QQ_CO2_B <- qq_data(uptake ~ Type + Treatment, data = CO2, line = "robust")
#'  plot(QQ_CO2_B)
#'
#' @importFrom graphics plot
#' @importFrom stats model.frame ppoints qnorm quantile sd
#'
plot.qqdata <- function(x, ..., use_colors = FALSE, scales = "free") {

    if (".group" %in%  colnames(x)) {
        if (use_colors){
        p <- ggplot(x,
                    aes(x, y,
                        ymin = ref_ci_lower,
                        ymax = ref_ci_upper,
                        col  = .group,
                        fill = .group
                    ))
        } else {
            p <- ggplot(x,
                        aes(x, y,
                            ymin = ref_ci_lower,
                            ymax = ref_ci_upper
                        ))
        }

        p <- p + facet_wrap( ~ .group, scales = scales)

    } else {
        p <- ggplot(x,
                    aes(x, y,
                        ymin = ref_ci_lower,
                        ymax = ref_ci_upper))

    }

    p +
        geom_line(aes(y = ref_y), lty = 2) +
        geom_point() +

        geom_ribbon(alpha = 0.2, col = NA) +
        geom_line(aes(y = ref_ci_lower), lty = 2) +
        geom_line(aes(y = ref_ci_upper), lty = 2) +

        labs(
            x = "Theoretical quantiles",
            y = "Empirical quantiles",
            color = "",
            fill = ""
        ) +
        ggtitle("QQ plot")
}
# =============================================================================

#' A qqplot for multiple groups with ggplot2
#'
#' @param x a numeric vector, a name of a vector in \code{data} or formula.
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
#' qq_plot(Sepal.Length ~ Species, data = iris)
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
                    distribution = "norm",
                    ...,
                    envelope = 0.95,
                    line = c("quartiles", "robust", "int=0,slope=1"),
                    labels = NULL,
                    groups = NULL,
                    data = NULL,
                    method = if (distribution == "norm") {"normal"} else {"any"},
                    use_colors = FALSE,
                    scales = "free") {

    qqdata <-  qq_data(x = x,
                 distribution = distribution,
                 ...,
                 envelope = envelope,
                 line = line,
                 labels = labels,
                 groups = groups,
                 data = data,
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
