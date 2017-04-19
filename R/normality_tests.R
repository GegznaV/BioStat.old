# =============================================================================
#' Normality tests with formula interface
#'
#' Perform Shapiro-Wilk (default),
#' Lilliefors (Kolmogorov-Smirnov), Anderson_darling and other tests of normality.
#'
#' @param x Either a formula or a numeric vector or a name of a vector
#'          in \code{data}.\cr
#'          If \code{x} is a formula (e.g. \code{variable ~ factor}), left
#'          side provides variable to be summarized. Right side and condition
#'          describe subsets. If the left side is empty, right side and
#'          condition are shifted over as a convenience.
#'
#' @param test (string | function) Either a string  (case insensitive, may be
#'             unambiguously abbreviated) with a name of nomality test or a
#'             function, which carries out the test.\cr
#' Possible names of tests:\itemize{
#'\item{"SW", "Shapiro-Wilk" — for Shapiro-Wilk test;}
#'\item{"Lilliefors" — for Kolmogorov-Smirnov test wtih Lilliefor's correction;}
#'\item{"AD", "Anderson-Darling" — for Anderson-Darling test;}
#'\item{"CVM", "CM", "Cramer-von Mises" — for Cramer-von Mises test;}
#'\item{"SF", "Shapiro-Francia" — for Shapiro-Francia test;}
#'\item{"Chi-squared","Pearsons" — for Pearson's chi-squared test of normality.}
#'}
#' @param ... Parameters to be passed to the main function
#'            (defined/selected in \code{fun}) that carries out a normality test.
#' @param data A data frame that contains the variables mentioned in \code{x}.
#'
#' @inheritParams mosaic::maggregate
#' @inheritParams stats::shapiro.test
#' @inheritParams nortest::lillie.test
#' @inheritParams nortest::pearson.test
#'
#'
#' @return  A data frame with normality test
#'          results for each group.
#'
#' @export
#'
#' @importFrom stats shapiro.test
#' @import nortest
#'
#' @seealso
#' Package \pkg{nortest}\cr
#'
#' @examples
#' library(BioStat)
#' library(pander)
#' data(CO2)
#'
#' rez <- test_normality(uptake ~ Type + Treatment,
#'                       data = CO2)
#' rez
#'
#' rez2 <- test_normality(uptake ~ Type + Treatment,
#'                        data = CO2,
#'                        test = "chi-sq")
#' rez2
#'
#' pander(rez)
#'
#' rez %>%
#'     prettify_p_column() %>%
#'     pander()
#'
#' format_numbers(rez, 3)
#'
#' rez %>% format_numbers(3)
#'
#' rez %>%
#'    format_numbers(3) %>%
#'    pander()

test_normality <- function(x,
                           data = NULL,
                           test = "Shapiro-Wilk",
                           ...

                           # , groups = NULL
                           # , sep = "|"
                           ) {
    if (is.function(test)) {
        FUN = test
    } else {
        # Possible choices
        tests = c(
            "SW", "Shapiro-Wilk",
            "Lilliefors",
            "AD", "Anderson-Darling",
            "CVM", "CM", "Cramer-von Mises",
            "SF", "Shapiro-Francia",
            "Chi-squared", "Pearsons"
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

            "chi-squared" = ,
            "pearsons"          = nortest::pearson.test

        )
    }

    test_(x,
          data = data,
          ...,
          groups = groups,
          FUN = FUN,
          sep = sep)

}


test_ <- function(x,
                  data = NULL,
                  ...,
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

        rez <- mosaic::maggregate(formula,
                           data = data,
                           FUN = FUN,
                           # sep = sep,
                           ...,
                           groups = groups,
                           .multiple = TRUE
                          )
        # remove strange format produced by `maggregate`
        ReduceC <- function(x) Reduce(c, x)
        rez <- mapply(ReduceC, rez) %>% data.frame()

        rez$data.name <- NULL # remove variable 'rez$data.name'
        rez$statistic  %<>%  readr::parse_number()
        rez$p.value    %<>%  readr::parse_number()

        class(rez) <- c("test_normality", "data.frame")
        return(rez)
    }

    FUN(x, ...)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SIGNIF <- function(x, digits = 3) {
    x %>%
        as.character() %>%
        as.numeric() %>%
        signif(digits = digits)
}

#' @export
#' @rdname test_normality
print.test_normality <- function(x, ..., digits = 3){

    method_of_test <- levels(x$method)
    x$method <- NULL

    if (is.numeric(x$statistic)) {
        x$statistic %<>% SIGNIF(digits)
    }

    if (is.numeric(x$p.value)) {
        x$p.value %<>% SIGNIF(digits) %>% as.character()
    }

    cat("\n", method_of_test, "\n\n")

    NextMethod(print, x)
}



