#' Calculate data for a qq-plot
#'
#' @param envelope (numeric) confidence level for point-wise confidence envelope.
#' @param line (string) A parameter, that controls how reference line is drawn.
#'            Options:\itemize{
#'            \item{\code{"0,1"} or \code{"int=0,slope=1"} to plot a line
#'             with intercept = 0 and slope = 1;}
#'            \item{ \code{"quartiles"} to pass a line through the quartile-pairs;}
#'            \item{ \code{"robust"} for a robust-regression line;
#'             the latter uses the \code{rlm} function in the \pkg{MASS} package};
#'            \item{option \code{"none"} is not implemented yet.}
#' }
#' @param method (string: \code{"trimmed-normal"}, \code{"normal"},
#'              \code{"any"}). A method, that controls how parameters for
#'              \code{distribution} are computed.
#'      Options:\itemize{
#'
#'            \item{\code{"mle-normal"} (default) all data values are used to
#'            calculate parameters of theoretical normal distribution using
#'            method of maximum likelihood;}
#'
#'            \item{\code{"trimmed-normal"} 10\% of the most extreme
#'            data values are trimmed before parameters of theoretical normal
#'            distribution are calculated using method of moments;}
#'
#'            \item{\code{"moment-normal"} all data values are used to calculate
#'            parameters of theoretical normal distribution using method of moments;}
#'
#'            \item{\code{"any"} parameters ate provided manually by user.}
#'      }
#'           Options \code{"mle-normal"}, \code{"trimmed-normal"} and
#'           \code{"moment-normal"} are applicable only if
#'           \code{distribution = "norm"}.
#'           Otherwise \code{"any"} is selected automatically.
#'
#'
#' @param ... Parameters to be passed to function, selected in \code{distribution}
#'
#' @inheritParams test_normality
#' @inheritParams car::qqPlot
#' @inheritParams mosaic::maggregate
#'
#' @return An object, which inherits from classes \code{qqdata} and
#'         \code{data.frame}. The object contains information, needed
#'         to plot a qqplot with reference line and its confidence intervals.
#'         These variables are contained: \itemize{
#'         \item\strong{x} – x axis values;
#'         \item\strong{y} – y axis values for points of qq plot;
#'         \item\strong{labels} – labels for each point;
#'         \item\strong{ref_y} – y axis values for reference line
#'         \item\strong{ref_ci_upper} and \strong{ref_ci_lower}
#'           – y axis values for upper and lower pointwise
#'            confidence interval of a reference line.
#'         }
#' @export
#'
#' @examples
#'
#' library(BioStat)
#' data(chickwts, package = "datasets")
#'
#' # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Input as formula + data:
#'
#' QQdata <- qq_data(~weight, data = chickwts)
#' head(QQdata)
#' coef(QQdata)
#'
#' # Column ".group" is added if applied by group:
#'
#' QQdata <- qq_data(weight ~ feed, data = chickwts)
#' head(QQdata)
#' coef(QQdata)
#'
#' qq_plot(weight ~ feed, data = chickwts)
#'
#' # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Input as variable name + data:
#'
#' QQdata <- qq_data("weight", data = chickwts)
#' head(QQdata)
#' coef(QQdata)
#'
#' QQdata <- qq_data("weight",groups = "feed", data = chickwts)
#' head(QQdata)
#' coef(QQdata)
#'
#' # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Input as vector
#'
#' QQdata <- qq_data(chickwts$weight)
#' head(QQdata)
#' coef(QQdata)
#'
#' # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Input as vector, several groups.
#' # Column ".group" is added
#'
#' QQdata <- qq_data(chickwts$weight, groups = chickwts$feed)
#' head(QQdata)
#' coef(QQdata)
#'
#'
#'
#' @seealso \code{\link[car]{qqPlot}} in \pkg{car} package,
#'  \code{\link[stats]{qqplot}} in \pkg{stats} package.
#'

# @import spMisc

qq_data <- function(x,
                    data = NULL,
                    distribution = "norm",
                    ...,
                    envelope = 0.95,
                    line = c("quartiles", "robust", "int=0,slope=1", "0,1", "none"),
                    labels = NULL,
                    groups = NULL,
                    method = c("mle-normal","trimmed-normal","moment-normal", "any")
                    )

{
    UseMethod("qq_data")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @export
#' @importFrom purrr "%||%"
qq_data.default <- function(x,
                     data = NULL,
                     distribution = "norm",
                     ...,
                     envelope = 0.95,
                     line = c("quartiles", "robust", "int=0,slope=1", "0,1", "none"),
                     labels = NULL,
                     groups = NULL,
                     method = c("mle-normal","trimmed-normal","moment-normal", "any")
)

{
    x      <- getVarValues(x, data)
    groups <- getVarValues(groups, data)

    names(x) <-
        if (is.null(labels)) {
            names(x) %||% seq(along = x) %>% as.character()
        } else {
            labels
        }

    method <- method[1]
    method <- match.arg(method)

    if (method == "trimmed-normal" & distribution == "norm") {
        qq_fun <- function(x){
            labels <- names(x)
            qq_data_(
                x = x,
                distribution = distribution,
                mean = mean(x,    trim = 0.1),
                sd   = sd(trim(x, trim = 0.1)),
                envelope = envelope,
                line   = line,
                labels = labels
            )
        }
    } else if (method == "moment-normal" & distribution == "norm") {
        qq_fun <- function(x){
            labels <- names(x)
            qq_data_(
                x = x,
                distribution = distribution,
                mean = mean(x),
                sd   = sd(x),
                envelope = envelope,
                line   = line,
                labels = labels
            )
        }
    } else if (method == "mle-normal" & distribution == "norm") {
        qq_fun <- function(x){
            labels <- names(x)
            params <- fitdistrplus::fitdist(x, "norm") %>% coef()
            qq_data_(
                x = x,
                distribution = distribution,
                mean = params["mean"],
                sd   = params["sd"],
                envelope = envelope,
                line   = line,
                labels = labels
            )
        }
    } else{
        qq_fun <- function(x){
            labels <- names(x)
            qq_data_(
                x = x,
                distribution = distribution,
                ...,
                envelope = envelope,
                line   = line,
                labels = labels
            )
        }
    }


    # If no groups exist
    if (is.null(groups)) {
        DF <- qq_fun(x)

    # Applied by group
    } else {
        DF <- tapply(x, groups, qq_fun)

        DF_attr <-
            purrr::map(DF, ~ attributes(.x)$refline)  %>%
            rbind_df_in_list()

        DF %<>% rbind_df_in_list()
        attr(DF, "refline") <- DF_attr
    }

    class(DF) <- c("qqdata", "data.frame")
    DF
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @export
# @rdname qq_data
qq_data.formula <- function(
    x,
    data = NULL,
    distribution = "norm",
    ...,
    envelope = 0.95,
    line = c("quartiles", "robust", "int=0,slope=1", "0,1", "none"),
    labels = NULL,
    groups = NULL,
    method = c("mle-normal","trimmed-normal","moment-normal", "any")
)
{
    DF <- model.frame(x, data = data)

    qq_main <- function(x,  groups = NULL)
        qq_data(
            x = x,
            distribution = distribution,
            ...,
            envelope = envelope,
            line   = line,
            labels = labels,
            groups = groups,
            method = method
        )

    if (length(x) == 2) {
        rez <- qq_main(DF[[1]])

    } else if (length(x) > 2) {
        rez <- qq_main(DF[[1]], groups = interaction(DF[-1], sep = " | "))

    } else {
        stop(
            "\nThe formula (`", x,"`) is incorrect.",
            "\nIt must contain at least 1 variable name."
        )
    }
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
qq_data_ <- function(x,
                     distribution = "norm",
                     ...,
                     envelope = 0.95,
                     line = c("quartiles", "robust", "int=0,slope=1", "0,1", "none"),
                     labels = if (!is.null(names(x))) names(x) else seq(along = x))

{
    line <- line[1]
    line <- match.arg(line)

    good <- !is.na(x)
    ord  <- order(x[good])

    ord_x   <- x[good][ord]
    ord_lab <- labels[good][ord]
    q_function <- eval_("q"  %++%  distribution)
    d_function <- eval_("d"  %++%  distribution)
    n <- length(ord_x)
    P <- ppoints(n)
    z <- q_function(P, ...)

    # Define a reference line
    switch(line,

           "quartiles" = {
               probs <- c(0.25, 0.75)

               Q_x <- stats::quantile(ord_x, probs)
               Q_z <- q_function(probs, ...)

               b <- (Q_x[2] - Q_x[1]) / (Q_z[2] - Q_z[1])
               a <- Q_x[1] - b * Q_z[1]
           },

           "robust" = {
               coef <- stats::coef(MASS::rlm(ord_x ~ z))
               a <- coef[1]
               b <- coef[2]
           },

           "0,1" = ,
           "int=0,slope=1" = {
               a <- 0
               b <- 1
           },

           "none" = {
               stop('`line = "none"` is not implemented yet.')
           }
    )

    ## !!! may be a mistake in `if (envelope == FALSE) 0.95`. Is `TRUE` correct?
    conf <- if (envelope == FALSE) 0.95 else envelope

    # Pointwise confidence interval
    zz <- qnorm(1 - (1 - conf) / 2)
    SE <- (b / d_function(z, ...)) * sqrt(P * (1 - P) / n)
    fit_value <- a + b * z
    upper <- fit_value + zz * SE
    lower <- fit_value - zz * SE

    # Output -------------------------------------------------------------
    data_points <-
        data.frame(
            x = z,
            y = ord_x,
            labels = ord_lab,
            ref_y  = fit_value,
            ref_ci_upper = upper,
            ref_ci_lower = lower
        )

    qq_attributes <- data.frame(
        intercept = a,
        slope = b,
        type = line,
        conf = conf
    )
    rownames(qq_attributes) <- "qq_refline"

    attr(data_points, "refline") <- qq_attributes
    return(data_points)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# =============================================================================
#' @param object A \code{qqdata} object.
#' @export
#' @method coef qqdata
#' @rdname qq_data
coef.qqdata <- function(object, ...) {
    attributes(object)$refline
}

# =======================================================
#' A method to plot a `qqdata` object as a qq-plot
#'
#' @param x A \code{qqdata} object
#' @param ... other parameters
#' @param scales ("free"|"free_x"|"free_y"|"fixed")
#'               a parmeter to be passed to
#'                \code{\link[ggplot2]{facet_wrap}}.
#' @param use_colors (logical) use colors for multiple groups
#' @import ggplot2
#' @examples
#' library(BioStat)
#' data(chickwts, package = "datasets")
#'
#' # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Input as formula + data:
#'
#' QQ_groups <- qq_data(weight ~ feed, data = chickwts)
#' plot(QQ_groups)
#'
#' QQ_groups <- qq_data(weight ~ feed, data = chickwts, method = "moment-normal")
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
#' @export
plot.qqdata <- function(x,
                        ...,
                        use_colors = FALSE,
                        scales = "free")

{
    if (".group" %in%  colnames(x)) {
        if (use_colors) {
            p <- ggplot(x,
                        aes(
                            x,
                            y,
                            ymin = ref_ci_lower,
                            ymax = ref_ci_upper,
                            col  = .group,
                            fill = .group
                        ))
        } else {
            p <- ggplot(x,
                        aes(x, y,
                            ymin = ref_ci_lower,
                            ymax = ref_ci_upper))
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

        labs(x = "Theoretical quantiles",
             y = "Empirical quantiles",
             color = "",
             fill = "") +
        ggtitle("QQ plot")
}
# =============================================================================
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## inputs:
#
# main = NULL,
# las = par("las"),

# col = palette()[1],
# col.lines = palette()[2],
# lwd = 2,
# pch = 1,
# cex = par("cex"),
#     ylab = deparse(substitute(x)),
# xlab = paste(distribution, "quantiles"

# id.method = "y",
# id.n = if (id.method[1] == "identify") Inf else 0,
# id.cex = 1,
# id.col = palette()[1],
# id.location = "lr",
# grid = TRUE
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# if (length(col) == length(x))        col <- col[good][ord]
# if (length(pch) == length(x))        pch <- pch[good][ord]
# if (length(cex) == length(x))        cex <- cex[good][ord]

# plot(
#     z,
#     ord_x,
#     type = "n",
#     xlab = xlab,
#     ylab = ylab,
#     main = main,
#     las = las
# )
#
# if (grid) {
#     grid(lty = 1, equilogs = FALSE)
#     box()
# }
#
# points(z,
#        ord_x,
#        col = col,
#        pch = pch,
#        cex = cex)

# qqlne coefs ------------------------------------------------------------


# if (line %in% c("quartiles", "robust"))
#     abline(a, b, col = col.lines, lwd = lwd)

# confidence interval of qqline -------------------------------------------


# if (envelope != FALSE) {
#     lines(z,
#           upper,
#           lty = 2,
#           lwd = lwd,
#           col = col.lines)
#
#     lines(z,
#           lower,
#           lty = 2,
#           lwd = lwd,
#           col = col.lines)
# }

# showLabels(
#     z,
#     ord_x,
#     labels = ord_lab,
#     id.method = id.method,
#     id.n = id.n,
#     id.cex = id.cex,
#     id.col = id.col,
#     id.location = id.location
# )
