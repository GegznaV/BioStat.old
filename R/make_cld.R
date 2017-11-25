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
#' obj2 <- with(OrchardSprays, pairwise.t.test(decrease, treatment))
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
#'
#' # Example 4: class `posthocTGH`
#'
#' obj4 <- posthoc_anova_tukey(weight ~ Diet, data = ChickWeight)
#' make_cld(obj4)
#'
#'
#' # Example 5: class `posthoc_tgh`
#'
#' obj5 <- posthoc_anova_games_howell(weight ~ Diet, data = ChickWeight)
#' make_cld(obj5)
#'
#'
#' # Example 6: class `formula`
#'
#' DataFrame <- data.table::fread(
#'     'Comparison     p.value p.adjust
#'     "EE - GB = 0"        1 1.000000
#'     "EE - CY = 0" 0.001093 0.003279
#'     "GB - CY = 0" 0.005477 0.008216')
#'
#' make_cld(p.adjust ~ Comparison, data = DataFrame)
#'
#'
#' # Example 7: class `matrix`
#' # (for symetric matrices of p values)
#'
#' # Create matrix
#' m <- c(1.00, 0.22, 0.05, 0.00,
#'        0.22, 1.00, 0.17, 0.01,
#'        0.05, 0.17, 1.00, 0.22,
#'        0.00, 0.01, 0.22, 1.00)
#' obj7 <- matrix(m, nrow = 4)
#' rownames(obj7) <- colnames(obj7) <- c("P", "O", "I", "U")
#' obj7
#'
#' # Make cld
#' make_cld(obj7)


# smokers  <- c(83, 90, 129, 70)
# patients <- c(86, 93, 136, 82)
# obj <- pairwise.prop.test(smokers, patients)
# make_cld(obj)

make_cld <- function(obj, ..., alpha = 0.05) {
    checkmate::assert_number(alpha, lower = 0, upper = 1)
    UseMethod("make_cld")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Helper function
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
    df <- pval_matrix_to_df(m1)
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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname make_cld
#' @export
make_cld.formula <- function(obj, ..., data = NULL, alpha = 0.05) {
    if (is.null(data)) {
        data <- rlang::f_env(obj)
    }
    res <- rcompanion::cldList(obj,
                               data = data,
                               threshold = alpha,
                               ...)

    update_cld(res)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname make_cld
#' @export
make_cld.matrix <- function(obj, ..., alpha = 0.05) {
    checkmate::assert_matrix(obj, mode = "numeric")

    if (is_square_matrix(obj)) {
        if (!all(colnames(obj) == rownames(obj))) {
            stop("Matrix is square but its column and row names does not match.")
        }
    } else {
        stop("This function works with square marices only.")
    }

    obj[upper.tri(obj, diag = TRUE)] <- NA
    df <- pval_matrix_to_df(obj)
    make_cld.pairwise_pval_df(df)

}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname make_cld
#' @export
make_cld.pairwise_pval_df <- function(obj, ..., alpha = 0.05) {
    res <- rcompanion::cldList(comparison = paste0(obj$gr1, " - ", obj$gr2),
                               p.value    = obj$p_values,
                               threshold  = alpha)
    update_cld(res)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
is_square_matrix <- function(x) {
    # From package `matrixcalc` version 1.0-3
    nrow(x) == ncol(x)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
is_symetric_matrix <- function(x) {
    checkmate::assert_matrix(x, mode = "numeric")
    if (!is_square_matrix(x)) stop("Matrix is not square")
    sum(x == t(x)) == length(x)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pval_matrix_to_df <- function(x) {
    if (is.null(colnames(x)))    colnames(x) <- seq_len(ncol(x))
    if (is.null(rownames(x)))    rownames(x) <- seq_len(nrow(x))

    df <- data.frame(
        gr1 = colnames(x)[col(x)],
        gr2 = rownames(x)[row(x)],
        p_values = c(x))

    df <- df[complete.cases(df), ]

    structure(df,
              class = c("pairwise_pval_df", "data.frame"))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~