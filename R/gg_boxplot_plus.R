#' [!] Plot a boxplot with additional components
#'
#' @param formula a formula with two variable names to analyze. First one is numeric, second one is a factor, e.g. \code{y ~ group}.
#' @param data a data frame with data.
#' @param cld a data frame with cld results (object of class \code{cld_object}).
#' @param sort_groups (\code{"no"}|\code{"yes"}|\code{"ascending"}|\code{"descending"}) Sort groups by position of median.
#' @param sort_fun A function that calculates one numeric statistic
#' (name without quotes). May be \code{median},
#' \code{mean}, \code{sd}, \code{var}, \code{IQR}, or similar.
#' @param add_points (\code{TRUE}|\code{FALSE})
#' @param add_mean_ci (\code{TRUE}|\code{FALSE})
#' @param add_median_ci (\code{TRUE}|\code{FALSE})
#' @param conf_level (numeric) Confidence level for confidence interval. Number from 0 to 1. Default is 0.95.
#' @param ci_boot_reps (numeric) Number of bootstrap repetitions for mean confidence interval calculation.
#' @param cld_y_adj,cld_y_mult (numeric) y position correction (addition and multiplication) factors for cld letters.
#' @param cld_color (character) Name of color for cld letters.
#'
#' @param ci_x_adj (numeric)  x position correction factor for mean confidence interval.
#' @param points_x_adj (numeric) x position correction factor for jittered points.
#' @param ... arguments to \code{sort_fun}.
#'
#' @return A ggplot2 plot object.
#' @export
#'
#' @keywords ggplot2 plots
#'
#' @examples
#' library(BioStat)
#'
#' # Example 1
#' gg_boxplot_plus(decrease ~ treatment, OrchardSprays)
#'
#'
#' # Example 2
#' gg_boxplot_plus(decrease ~ treatment, OrchardSprays,
#'                 sort_groups = "descending")
#'
#'
#' # Example 3a
#'
#' res <- posthoc_anova(weight ~ Diet, data = ChickWeight)
#' cld_result <- make_cld(res)
#'
#' gg_boxplot_plus(weight ~ Diet, data = ChickWeight,
#'                 cld = cld_result)
#'
#' # Example 3b
#'
#' gg_boxplot_plus(weight ~ Diet, data = ChickWeight,
#'                 cld = cld_result,
#'                 sort_groups = "descending",
#'                 sort_fun = mean)
#'
#' # Example 3c: do simple transformations
#'
#' gg_boxplot_plus(log(weight) ~ Diet, data = ChickWeight,
#'                 sort_groups = "descending",
#'                 sort_fun = mean)
#'
#' # Example 3d: facetting
#'
#' gg_boxplot_plus(weight ~ as.factor(Time), data = ChickWeight) +
#'     facet_wrap("Diet")
#'
#' # Example 4
#'
#' res2 <- posthoc_anova(decrease ~ treatment, data = OrchardSprays)
#' cld_result2 <- make_cld(res2)
#'
#' gg_boxplot_plus(decrease ~ treatment, data = OrchardSprays,
#'                 cld = cld_result2,
#'                 sort_groups = "descending",
#'                 sort_fun = mean)
#'
#' gg_boxplot_plus(decrease ~ treatment, data = OrchardSprays,
#'                 cld = cld_result2,
#'                 sort_groups = "ascending",
#'                 sort_fun = IQR)
#'
gg_boxplot_plus <- function(
    formula,
    data = NULL,
    cld = NULL,

    sort_groups = c("no", "yes", "ascending", "descending"),
    sort_fun = median,

    add_points = TRUE,
    add_mean_ci = TRUE,
    add_median_ci = FALSE,

    conf_level = 0.95,
    ci_boot_reps = 999,

    cld_color = "black",
    cld_y_adj  = 0,
    cld_y_mult = 0.05,
    ci_x_adj = -0.3,
    points_x_adj =  0.3,

    ...

) {

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Prepare data
    if (!is.null(cld)) {
        checkmate::assert_class(cld, "cld_object")
    }

    # if (is.null(data)) {
    #     data <- rlang::f_env(formula)
    # }
    #
    # fctr_name <- all.vars(formula[[3]])
    #    y_name <- all.vars(formula[[2]])
    #
    # DATA <- dplyr::select(data,
    #                       y = !!rlang::sym(y_name),
    #                       group = !! rlang::sym(fctr_name))
    #
    # DATA <- dplyr::mutate(DATA, group = factor(group))

    obj <- parse_formula(formula, data, keep_all_vars = TRUE)
    y_name <- obj$names$y
    fctr_name <- obj$names$x

    DATA <- dplyr::select(obj$data,
                          .y = !! rlang::sym(y_name),
                          .group = !! rlang::sym(fctr_name),
                          dplyr::everything())

    sort_groups <- match.arg(sort_groups)
    desc <- switch(sort_groups,
           "yes" = ,
           "ascending" = FALSE,
           "descending" = TRUE,
           NULL)

    if (!is.null(desc)) {
        DATA <- dplyr::mutate(
            DATA,
            .group = forcats::fct_reorder(.group,
                                          .y,
                                          fun = sort_fun,
                                          ...,
                                          .desc = desc))
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Plot

    p <- ggplot(DATA, aes(x = .group, y = .y, fill = .group)) +
        geom_boxplot(width = .2, notch = add_median_ci)

    # p <- ggplot(DATA, aes(x = .group, y = .y, fill = .group)) +
    #     geom_violin(aes(color = .group), fill = NA, width = .6, lwd = 1) +
    #     geom_boxplot(width = .1, notch = add_median_ci)

    if (add_points) {
        p <- p +
            geom_jitter(
                aes(x = as.numeric(.group) + points_x_adj),
                alpha = 0.3,
                width = .08,
                shape = 21)
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Add mean CI ------------------------------------------------------------
    if (add_mean_ci) {
        # mean_ci <- dplyr::do(dplyr::group_by(DATA, .group),
        #                      ci_mean_boot(.$.y, repetitions = ci_boot_reps,
        #                                    conf_level = conf_level))
        #
        # p <- p +
        #     geom_errorbar(data = mean_ci,
        #                   aes(x = as.numeric(.group) + ci_x_adj,
        #                       ymin = lower,
        #                       ymax = upper,
        #                       color = .group),
        #                   inherit.aes = FALSE,
        #                   width = 0.1) +
        #
        #     geom_point(data = mean_ci, shape = 21, color = "black",
        #                aes(x = as.numeric(.group) + ci_x_adj,
        #                    y = mean,
        #                    fill = .group),
        #                inherit.aes = FALSE)

        p <- p +
            stat_summary(aes(x = as.numeric(.group) + ci_x_adj,
                             color = .group),
                         geom = "errorbar",
                         fun.data = mean_cl_boot,
                         fun.args = list(
                             conf.int = conf_level,
                             B = ci_boot_reps),
                         width = 0.1) +

            stat_summary(aes(x = as.numeric(.group) + ci_x_adj,
                             color = .group),
                         geom = "point",
                         fun.y = mean)

    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Add cld, if provided ---------------------------------------------------
    if (!is.null(cld)) {

        y_upp <- max(DATA[[".y"]], na.rm = TRUE)
        y_low <- min(DATA[[".y"]], na.rm = TRUE)

        cld_y_pos <- cld_y_adj + y_upp + cld_y_mult*(y_upp - y_low)

        cld <- dplyr::mutate(cld,
                             group = factor(group, levels = levels(DATA$.group)))

        p <- p +
            geom_text(data = cld,
                      color = cld_color,
                      aes(x = group, label = cld, y = cld_y_pos),
                      fontface = "bold",
                      inherit.aes = FALSE)
    }

    # Add labels -------------------------------------------------------------
    p <- p +
        labs(x = fctr_name, y = y_name, fill = fctr_name, color = fctr_name) +
        theme_bw()

    # Output -----------------------------------------------------------------
    p
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
vertical_cld <- function(iii) {
    iii <- as.character(iii)
    # iii <- gsub("_", " ", iii)
    sapply(iii, function(x) paste(strsplit(x,"")[[1]], "\n", collapse = ""))
}


# geom_text(data = cld_result,
#           aes(x = group, label = vertical_cld(spaced_cld), y = cld_y_),
#           fontface = "bold",
#           inherit.aes = FALSE,
#           vjust = 0) +
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

