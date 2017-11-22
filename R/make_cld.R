#' Make a compact letter display
#'
#' Make a compact letter display for results of pair-wise comparisons (e.g., ANOVA post-hoc tests, Kruskal-Walis post-hoc tests and other).
#'
#' This function is based on function \code{\link[rcompanion]{cldList}()} from package \pkg{rcompanion}.
#'
#'
#' @param obj Object with pair-wise comparisoms (e.g., post-hoc test results).
#' @param ... Further arguments to methods.
#'
#' @return (A dataframe with) compact letter display.
#' @export
#'
# @examples

make_cld <- function(obj, ..., alpha = 0.05) {
    UseMethod("make_cld")
}



#' @rdname make_cld
#' @export
make_cld.posthocTGH <- function(obj, ..., alpha = obj$intermediate$alpha) {

    which_posthoc <-
        switch(tolower(obj$input$method),
               "games-howell" = "games.howell",
               "tukey"        = "tukey"
        )

    res <- rcompanion::cldList(
        comparison = obj$intermediate$pairNames,
        p.value    = obj$output[[which_posthoc]]$p.adjusted,
        threshold  = obj$intermediate$alpha,
        ...)
    res$Letter2 <- gsub(" ", "_", res$MonoLetter)

    res
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname make_cld
#' @export
make_cld.PMCMR <- function(obj, ..., alpha = 0.05) {

    m1 <- obj$p.value

    df <- data.frame(
        gr1 = colnames(m1)[col(m1)],
        gr2 = rownames(m1)[row(m1)],
        p_values = c(m1))

    df <- df[complete.cases(df), ]


    rcompanion::cldList(comparison = paste0(df$gr1, " - ", df$gr2),
                        p.value    = df$p_values,
                        threshold  = alpha,
                        ...)
}