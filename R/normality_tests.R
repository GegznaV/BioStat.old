# =============================================================================
#' Normality tests with formula interface
#'
#' Performs the Shapiro-Wilk (default), Lilliefors (Kolmogorov-Smirnov) and other test of normality.
#'
#' @inheritParams stats::shapiro.test
#' @inheritParams nortest::lillie.test
#' @inheritParams nortest::pearson.test
#' @inheritParams mosaic::maggregate
#' @param test (string | function) Either one of the following string (case insensitive, may be unambiguously abbreviated) which describes a desired test of normality:\cr
#'"SW", "Shapiro-Wilk",\cr
#'"Lilliefors",\cr
#'"AD", "Anderson-Darling",\cr
#'"CVM", "CM", "Cramer-von Mises",\cr
#'"SF", "Shapiro-Francia",\cr
#'"Chi-square","Pearson"  \cr
#'
#'or a function, which carries out the test.
#'
#'
#' @return  A data frame with results for each group
#'
#'                  /NOT DESCRIBED YET/
#'
#'
#' @export
#' @importFrom stats shapiro.test
#' @import nortest
#' @examples
#' library(BioStat)
#' data(CO2)
#' test_normality(uptake ~ Type + Treatment, data = CO2)
#'
#' test_normality(uptake ~ Type + Treatment, "chi-sq", data = CO2)
test_normality <- function(x,
                           test = "Shapiro-Wilk",
                           ...,
                           data = NULL,
                           groups = NULL
                           # , sep = "|"
                           ) {

    if (is.function(test)) {
        FUN = test
    } else {
        # Possible choices
        tests = c(
            "SW",
            "Shapiro-Wilk",
            "Lilliefors",
            "AD",
            "Anderson-Darling",
            "CVM",
            "CM",
            "Cramer-von Mises",
            "SF",
            "Shapiro-Francia",
            "Chi-square",
            "Pearson"
        )  %>% tolower()

        test <- match.arg(tolower(test), tests)

        FUN = switch(test,
            "sw" = ,
            "shapiro-wilk"     = stats::shapiro.test,

            "lilliefors"       = nortest::lillie.test,

            "ad" = ,
            "anderson-darling" = nortest::ad.test,

            "cvm" = ,
            "cm"  = ,
            "cramer-von mises" = nortest::cvm.test,

            "sf" = ,
            "shapiro-francia"  = nortest::sf.test,

            "chi-square" = ,
            "pearson"          = nortest::pearson.test

        )
    }

    test_(x, ..., data = data, groups = groups, FUN = FUN, sep = sep)

}


# setGeneric("shapiro.test")
# methods("shapiro.test")

test_ <- function(x,
                  ...,
                  data = NULL,
                  groups = NULL,
                  FUN = stats::shapiro.test,
                  sep = "|")
    # na.rm = getOption("na.rm", FALSE)
{
    if (lazyeval::is_formula(x)) {
        if (is.null(data)) {
            data <- lazyeval::f_env(x)
        }

        formula <- mosaic_formula_q(x, groups = groups, max.slots = 3)

        rez <- mosaic:::maggregate(formula,
                           data = data,
                           FUN = FUN,
                           # sep = sep,
                           ...,
                           .multiple = TRUE
                          )
        # remove strange format produced by `maggregate`
        ReduceC <- function(x) Reduce(c, x)
        rez <- mapply(ReduceC, rez) %>% data.frame

        rez$data.name <- NULL # remove variable 'rez$data.name'
        class(rez) <- c("normality_test", "data.frame")
        return(rez)
    }

    FUN(x, ...)
}

SIGNIF <- function(x, digits = 3) {
    x %>%
        as.character() %>%
        as.numeric() %>%
        signif(digits = digits)
}

#' Print `normality_test` object
#'
#' @param obj An object of class \code{normality_test}.
#' @param digits (numeric) number of significant digits to display.
#'                Default is 3.
#' @param ...
#'
#' @export
#' @method print normality_test
#' @rdname test_normality
print.normality_test <- function(obj, ..., digits = 3){

    method_of_test <- levels(obj$method)
    obj$method <- NULL

    obj$statistic %<>% SIGNIF(digits)
    obj$p.value   %<>% SIGNIF(digits) %>% as.character()

    cat("\n", method_of_test, "\n\n")

    NextMethod(print, obj)
}



