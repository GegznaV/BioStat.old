#' Make a compact letter display for pair-wise comparison
#'
#' Make a compact letter display for results of pair-wise comparisons (e.g., ANOVA post-hoc tests, Kruskal-Wallis post-hoc tests and other).
#'
#' This function is based on function \code{\link[rcompanion]{cldList}()} from package \pkg{rcompanion}.
#'
#'
#' @param obj Object with pair-wise comparisons (e.g., post-hoc test results).
#'   Currently supported objects: \itemize{
#'   \item \emph{posthocTGH} from package \pkg{userfriendlyscience};
#'   \item \emph{PMCMR} from package \pkg{PMCMR}.
#'   }
#' @param ... Further arguments to methods.
#' @param alpha (numeric from 0 to 1) Significance level.
#'
#' @return (A data frame with) compact letter display.
#' @export
#'
#' @examples
#' # Example 1: class `pairwise.htest`
#'
#' obj1 <- pairwise.wilcox.test(chickwts$weight, chickwts$feed, exact = FALSE)
#' make_cld(obj1)
#'
#'
#' # Example 2: class `pairwise.htest`
#'
#' obj2 <- pairwise.t.test(OrchardSprays$decrease, OrchardSprays$treatment)
#' make_cld(obj2)
#'
#'
#' # Example 3: class `PMCMR`
#'
#' \donttest{
#' obj3 <- PMCMR::posthoc.kruskal.conover.test(count ~ spray,
#'                                             data = InsectSprays)
#' make_cld(obj3)
#' }
#'
#' # Example 4: class `posthocTGH`
#'
#' obj4 <- posthoc_tgh(ChickWeight$weight,
#'                     ChickWeight$Diet,
#'                     method = "tukey")
#' make_cld(obj4)
#'
#'
#' # Example 5: class `posthoc_tgh`
#'
#' obj5 <- posthoc_tgh(ChickWeight$weight,
#'                     ChickWeight$Diet,
#'                     method = "games-howell")
#' make_cld(obj5)
#'

# smokers  <- c(83, 90, 129, 70)
# patients <- c(86, 93, 136, 82)
# obj <- pairwise.prop.test(smokers, patients)
# make_cld(obj)

make_cld <- function(obj, ..., alpha = 0.05) {
    checkmate::assert_number(alpha, lower = 0, upper = 1)
    UseMethod("make_cld")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
update_cld <- function(res) {
    res$MonoLetter <- gsub(" ", "_", res$MonoLetter)
    names(res) <- c("Group", "cld", "mono_cld")
    structure(res, class = c("cld_object", class(res)))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname make_cld
#' @export
make_cld.pairwise.htest <- function(obj, ..., alpha = 0.05) {

    m1 <- obj$p.value

    df <- data.frame(
        gr1 = colnames(m1)[col(m1)],
        gr2 = rownames(m1)[row(m1)],
        p_values = c(m1))

    df <- df[complete.cases(df), ]

    res <- rcompanion::cldList(comparison = paste0(df$gr1, " - ", df$gr2),
                               p.value    = df$p_values,
                               threshold  = alpha)
    update_cld(res)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname make_cld
#' @export
make_cld.posthocTGH <- function(obj, ..., alpha = obj$intermediate$alpha) {

    which_posthoc <-
        switch(tolower(obj$input$method),
               "games-howell" = "games.howell",
               "tukey"        = "tukey",
               stop("Incorrect method selected: ", obj$input$method)
        )

    res <- rcompanion::cldList(comparison = obj$intermediate$pairNames,
                               p.value    = obj$output[[which_posthoc]]$p.adjusted,
                               threshold  = obj$intermediate$alpha,
                               ...)
    update_cld(res)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname make_cld
#' @export
make_cld.posthoc_tgh <- function(obj, ..., alpha = obj$intermediate$alpha) {

    which_posthoc <-
        switch(tolower(obj$input$method),
               "games-howell" = "games.howell",
               "tukey"        = "tukey",
               stop("Incorrect method selected: ", obj$input$method)
        )

    res <- rcompanion::cldList(comparison = obj$intermediate$pairNames,
                               p.value    = obj$output[[which_posthoc]]$p.adjusted,
                               threshold  = obj$intermediate$alpha,
                               ...)
    update_cld(res)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname make_cld
#' @export
make_cld.PMCMR <- function(obj, ..., alpha = 0.05) {
    make_cld.pairwise.htest(obj, ..., alpha = alpha)
}
