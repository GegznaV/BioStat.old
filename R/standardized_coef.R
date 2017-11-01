#' Compute Standardized Regression Coefficients
#'
#' Compute the standardized regression coefficients (beta) from an object of class \code{lm}).
#'
#' @param obj (\code{lm} object) A result of function \code{lm()}.
#'
#' @return
#' Object of class \code{lm_beta} which is a named numeric vector  that represents each standardized coefficient from \code{lm()} model.
#'
#'
#' @details
#' This function is inspired by function  \code{\pkg{QuantPsyc}::lm.beta()} written by Thomas D. Fletcher. \cr
#' \code{standardized_coef()} provides standardized coefficients even when interaction members are present. This is achieved by computing whole model matrix (with all right-hand side members of formula used in call of \code{lm()}) and calculating standard deviations of each regressor (including interaction members) based on these columns. The remaining calculations are the same as in \code{\pkg{QuantPsyc}::lm.beta()}.
#'
#' @author
#' Vilmantas Gegzna (modified function written by Thomas D. Fletcher).
#'
#' @seealso
#' \itemize{
#'     \item \code{\link[stats]{lm}} in package \pkg{stats}.
#'     \item \code{\link[QuantPsyc]{lm.beta}} in package \pkg{QuantPsyc}.
#' }
#' @keywords models
#' @export
#' @examples
#'
#' data(USJudgeRatings)
#' us <- USJudgeRatings
#' names(us)
#'
#' lm1 <- lm(CONT ~ INTG + DMNR + log(DILG), data = us)
#' standardized_coef(lm1)
#'
#' lm2 <- lm(CONT ~ INTG + DMNR*DILG, data = us)
#' standardized_coef(lm2)
#'

standardized_coef <- function(obj) {
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    checkmate::assert_class(obj, "lm")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          b <- summary(obj)$coef[-1, 1]
    b_names <- rownames(summary(obj)$coef)[-1]
    # Extracts all members of right-hand side of formulae:
    data_ <- model.matrix(as.formula(obj$call$formula), data = obj$model)
    # Make correct order of columns:
    data_ <- as.data.frame(data_)[, b_names, drop = FALSE]
    # Do the standardization:
      sx_ <- sapply(data_, sd)
      sy_ <- sapply(obj$model[1], sd)
    beta  <- b * sx_ / sy_
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    structure(beta,
              class = c("lm_beta","numeric"))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}

#' @rdname standardized_coef
#'
#' @param x \code{lm_beta} object.
#' @param digits (integer) number of decimal places to round the answer to.
#'               Default is 3.
#' @param ... further parameters to \code{print} method.
#'
#' @export

print.lm_beta <- function(x, ..., digits = 3) {
    cat("Standardized Regression Coefficients:\n")
    print(unclass(round(x,  digits = digits)), ...)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # # This part is the same as QuantPsyc::lm.beta() ~~~~~~~~~~~~~~~~~~~~~~
# b <- summary(obj)$coef[-1, 1]
#
# sx <- sapply(obj$model[-1], sd)
# sy <- sapply(obj$model[1],  sd)
# beta_0 <- b * sx /  sy
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~