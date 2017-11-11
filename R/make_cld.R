#' Make a compact letter display
#'
#' Make a compact letter display for results of ANOVA post-hoc test.
#'
#' @param obj Object with post-hoc results
#' @param ... Further arguments to methods.
#'
#' @return (A dataframe with) compact letter display.
#' @export
#'
# @examples

make_cld <- function(obj, ...) {
    UseMethod("make_cld")
}



#' @rdname make_cld
#' @export
make_cld.posthocTGH <- function(obj, ...) {

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