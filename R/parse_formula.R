# Based on:
# <environment: namespace:mosaicCore>
# [!!!]
formula_parts <- function(formula, ...) {
    op <- formula[[1]]

    condition <- NULL
    if (length(formula) == 2) {
        lhs <- NULL
        rhs <- formula[[2]]

    } else if (length(formula) == 3) {
        lhs <- formula[[2]]
        rhs <- formula[[3]]

    } else {
        stop("Invalid formula type.")
    }

    if (inherits(rhs, "call") && rhs[[1]] == "|") {
        condition <- rhs[[3]]
        rhs <- rhs[[2]]
    }

    # Output  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    structure(
        list(op = op,
             lhs = lhs,
             rhs = rhs,
             condition = condition),
        class = "parsedFormula"
    )

}
