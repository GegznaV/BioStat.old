#' Make a data frame with cld
#'
#' Function is based on \pkg{rcompanion}\code{::cldList}.
#' @inheritParams rcompanion::cldList
#'
#' @export
#' @keywords internal

# @param comparison_order (character) Character vector with desired order of groups.
make_cld_df <- function(
    formula      = NULL,
    data         = NULL,
    comparison   = NULL,
    p.value      = NULL,
    threshold    = 0.05,
    print.comp   = FALSE,
    remove.space = TRUE,
    remove.equal = TRUE,
    # remove.zero  = TRUE,
    swap.colon   = TRUE,
    swap.vs      = FALSE,
    # comparison_order = NULL,
    ...
)
{
    if (!is.null(formula)) {
        p.value    = eval(parse(text = paste0("data", "$", all.vars(formula[[2]])[1])))
        comparison = eval(parse(text = paste0("data", "$", all.vars(formula[[3]])[1])))
        # p.value    = eval_glue("data${all.vars(formula[[2]])[1]}")
        # comparison = eval_glue("data${all.vars(formula[[3]])[1]}")
    }


    Comparison = p.value < threshold
    # if (sum(Comparison) == 0) {
    #     stop("No significant differences.", call. = FALSE)
    # }
    if (remove.space == TRUE) {
        comparison = gsub(" ", "", comparison)
    }
    if (remove.equal == TRUE) {
        comparison = gsub("=", "", comparison)
    }
    # if (remove.zero == TRUE) {
    #     comparison = gsub("0", "", comparison)
    # }
    if (swap.colon == TRUE) {
        comparison = gsub(":", "-", comparison)
    }
    if (swap.vs == TRUE) {
        comparison = gsub("vs", "-", comparison)
    }
    names(Comparison) = comparison
    if (print.comp == TRUE) {
        Y = data.frame(Comparisons = names(Comparison))
        cat("\n\n")
        print(Y)
        cat("\n\n")
    }
    MCL = multcompView::multcompLetters(Comparison)

    regular_cld <- as.character(MCL$Letters)
    spaced_cld  <- as.character(MCL$monospacedLetters)

    if (is.null(MCL$monospacedLetters)) {
        spaced_cld = regular_cld
    } else {
        spaced_cld = spaced_cld
    }

    res = data.frame(group      = names(MCL$Letters),
                     cld        = as.character(MCL$Letters),
                     spaced_cld = gsub(" ", "_", spaced_cld))

    structure(res, class = c("cld_object", class(res)))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Helper function
# cld_update <- function(res) {
#     res$MonoLetter <- gsub(" ", "_", res$MonoLetter)
#     names(res) <- c("Group", "cld", "spaced_cld")
#     structure(res, class = c("cld_object", class(res)))
# }