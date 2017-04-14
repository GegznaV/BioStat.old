#' Calculate data for a qqplot
#'
#' @inheritParams car::qqPlot
#' @param obj qqdata object.
#'
#' @return An object, which inherits from classes \code{qqdata} and
#'         \code{data.frame}. The object contains information, needed
#'         to plot a qqplot with reference line and its confidence intervals.
#'         These variables are contained: \itemize{
#'         \item\strong{x} x axis values;
#'         \item\strong{y} y axis values for points of qq plot;
#'         \item\strong{labels} labels for each point;
#'         \item\strong{ref_y} y axis values for reference line
#'         \item\strong{ref_ci_upper} and \strong{ref_ci_lower} y axis values
#'          for upper and lower pointwise confidence interval of reference line.
#'         \item
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
#'  \code{\link[stats]{qqplot}} in \pkg{stats} package
#'
#' @import spMisc

qq_data <- function (x,
                     distribution = "norm",
                     ...,
                     envelope = 0.95,
                     line = c("quartiles", "robust", "int=0,slope=1"),
                     labels = NULL,
                     groups = NULL,
                     data = NULL,
                     method = if (distribution == "norm") "normal" else "any")

{
    UseMethod("qq_data")
}



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @export
#' @rdname qq_data
#' @importFrom purrr "%||%"
qq_data.default <- function (x,
                     distribution = "norm",
                     ...,
                     envelope = 0.95,
                     line = c("quartiles", "robust", "int=0,slope=1"),
                     labels = NULL,
                     groups = NULL,
                     data = NULL,
                     method = if (distribution == "norm") "normal" else "any")

{
    x      <- getVarValues(x, data)
    groups <- getVarValues(groups, data)

    names(x) <-
        if (is.null(labels)) {
            names(x) %||% seq(along = x) %>% as.character()
        } else {
            labels
        }

    if (method == "normal" & distribution == "norm"){
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
    } else {
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
    if (is.null(groups)){
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
#' @rdname qq_data
qq_data.formula <- function(
    x,
    distribution = "norm",
    ...,
    envelope = 0.95,
    line = c("quartiles", "robust", "int=0,slope=1"),
    labels = NULL,
    groups = NULL,
    data = NULL,
    method = if (distribution == "norm") "normal" else "any"
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
qq_data_ <- function (x,
                     distribution = "norm",
                     ...,
                     envelope = 0.95,
                     line = c("quartiles", "robust", "int=0,slope=1"),
                     labels = if (!is.null(names(x))) names(x) else seq(along = x))

{
    line <- match.arg(line)
    line <- line[1]

    good <- !is.na(x)
    ord  <- order(x[good])

    ord_x   <- x[good][ord]
    ord_lab <- labels[good][ord]
    q_function <- eval_("q"  %++%  distribution)
    d_function <- eval_("d"  %++%  distribution)
    n <- length(ord_x)
    P <- ppoints(n)
    z <- q_function(P, ...)

    switch(line,
           "quartiles" = {
               quantiles <- c(0.25, 0.75)

               Q_x <- quantile(ord_x, quantiles)
               Q_z <- q_function(quantiles, ...)

               b <- (Q_x[2] - Q_x[1]) / (Q_z[2] - Q_z[1])
               a <- Q_x[1] - b * Q_z[1]
           },
           "robust" = {
               coef <- coef(MASS::rlm(ord_x ~ z))
               a <- coef[1]
               b <- coef[2]
           },
           "int=0,slope=1" = {
               a <- 0
               b <- 1
           })

    conf <- if (envelope == FALSE) 0.95    else envelope

    zz <- qnorm(1 - (1 - conf) / 2)
    SE <- (b / d_function(z, ...)) * sqrt(P * (1 - P) / n)
    fit_value <- a + b*z
    upper <- fit_value + zz*SE
    lower <- fit_value - zz*SE

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
trim <- function(x, trim = 0.1){
    p <- quantile(x, probs = c(trim/2, 1-trim/2))
    x[x > p[1] & x < p[2]]
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

eval_ <-
    function(x, envir = parent.frame(), ...) {
        eval(parse(text = x), envir = envir, ...)
    }
# =============================================================================
#' @export
#' @rdname qq_data
coef.qqdata <- function(obj) {
    attributes(obj)$refline
}


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
