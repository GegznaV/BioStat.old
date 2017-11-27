# library(BioStat)
# library(PMCMR)
#
# model_tukey <- posthoc.kruskal.conover.test(decrease ~ treatment, data = OrchardSprays)
#
# model_tukey
#
# cld_result <- make_cld(model_tukey)
# cld_result
# pander::pander(cld_result)
#
# library(tidyverse)

# OrchardSprays
# cld_max <- with(OrchardSprays, tapply(decrease, treatment, function(x) 1.05 * max(x)))
# cld_y <- max(-OrchardSprays$decrease * 0.95)
# cld_y <- min(-OrchardSprays$decrease * 1.05)
#
# cld_y <- min(OrchardSprays$decrease * 0.95)
# cld_y <- max(OrchardSprays$decrease * 1.05)






#
# library(tidyverse)
#
# cld = cld_result
# add_points = TRUE
# add_mean_ci = TRUE
# add_median_ci = FALSE
#
# cld_y_adj = 1.05
# ci_x_adj = -0.3
# ci_boot_reps = 2000
#
# points_x_adj =  0.3

#' Plot a boxplot with additional components
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
#' @param ci_boot_reps (numeric) Number of bootstrap repetitions for mean confidence interval calculation.
#' @param cld_y_adj (numeric) y position correction factor for cld letters.
#' @param cld_color (character) Name of color for cld letters.
#'
#' @param ci_x_adj (numeric)  x position correction factor for mean confidence interval.
#' @param points_x_adj (numeric) x position correction factor for jittered points.
#' @param ... arguments to \code{sort_fun}.
#'
#' @return A ggplot2 plot object.
#' @export
#'
#' @examples
#' # Example 1
#' gg_boxplot_plus(decrease ~ treatment, OrchardSprays)
#'
#'
#' # Example 2
#' gg_boxplot_plus(decrease ~ treatment, OrchardSprays, sort_groups = "yes")
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

    ci_boot_reps = 999,

    cld_color = "black",
    cld_y_adj = 1.05,
    ci_x_adj = -0.3,
    points_x_adj =  0.3,

    ...


) {

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Prepare data
    if (!is.null(cld)) {
        checkmate::assert_class(cld, "cld_object")
    }

    if (is.null(data)) {
        data <- rlang::f_env(formula)
    }

    sort_groups <- match.arg(sort_groups)

    fctr_name <- all.vars(formula[[3]])
       y_name <- all.vars(formula[[2]])

    DATA <- dplyr::select(data,
                          y = !!rlang::sym(y_name),
                          group = !! rlang::sym(fctr_name))

    DATA <- dplyr::mutate(DATA, group = factor(group))

    switch(sort_groups,
           "yes" = ,
           "ascending" = {
               DATA <- dplyr::mutate(DATA,
                                     group = forcats::fct_reorder(group,
                                                                  y,
                                                                  fun = sort_fun,
                                                                  ...,
                                                                  .desc = FALSE))
           },
           "descending" = {
               DATA <- dplyr::mutate(DATA,
                                     group = forcats::fct_reorder(group,
                                                                  y,
                                                                  fun = sort_fun,
                                                                  ...,
                                                                  .desc = TRUE))
           })


    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Plot

    p <- ggplot(DATA, aes(x = group, y = y, fill = group)) +
        geom_boxplot(width = .2, notch = add_median_ci)

    if (add_points) {
        p <- p +
            geom_jitter(
                aes(x = as.numeric(group) + points_x_adj),
                alpha = 0.3,
                width = .08,
                shape = 21)
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (add_mean_ci) {
        mean_ci <- dplyr::do(dplyr::group_by(DATA, group),
                             ci_mean_boot( .$y, repetitions = ci_boot_reps))

        p <- p +
            geom_errorbar(data = mean_ci,
                          aes(x = as.numeric(group) + ci_x_adj,
                              ymin = lower,
                              ymax = upper,
                              color = group),
                          inherit.aes = FALSE,
                          width = 0.1) +

            geom_point(data = mean_ci, shape = 21, color = "black",
                       aes(x = as.numeric(group) + ci_x_adj, y = mean, fill = group),
                       inherit.aes = FALSE)

    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (!is.null(cld)) {
        cld_y <- max(DATA[["y"]] * cld_y_adj, na.rm = TRUE)

        cld <- dplyr::mutate(cld,
                             group = factor(group, levels = levels(DATA$group)))

        p <- p +
            geom_text(data = cld,
                      color = cld_color,
                      aes(x = group, label = cld, y = cld_y),
                      fontface = "bold",
                      inherit.aes = FALSE)
    }


    p <- p +
        labs(x = fctr_name, y = y_name) +
        theme_bw()

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

