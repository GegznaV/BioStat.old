# =============================================================================
#' Normality tests with formula interface
#'
#' Perform Shapiro-Wilk (default),
#' Lilliefors (Kolmogorov-Smirnov), Anderson_darling and other tests of normality.
#'
#' @param x Either a formula or a numeric vector or a name of a vector
#'          in \code{data}.\cr
#'          If \code{x} is a formula (e.g. \code{variable ~ factor}), left-hand
#'          side provides variable to be summarized. Right-hand side and condition
#'          describe subsets. If the left-hand side is empty, right-hand side and
#'          condition are shifted over as a convenience.
#'
#' @param data A data frame that contains the variables mentioned in \code{x}.
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
#' # Round to 3 decimal digits (all columns):
#' format_numbers(rez, 3)
#'
#' # Round to 3 significant digits (all columns):
#' format_numbers(rez, 3, format = "g")
#'
#' # Prettify the column with p-values
#' prettify_p_column(rez)
#'
#' # Print as a 'pandoc' table
#' pander(rez)
#'
#' # Other output possibilities
#' rez %>% format_numbers(3)
#'
#' rez %>%
#'    format_numbers(3) %>%
#'    pander()
#'
#' rez %>%
#'     format_numbers(3, "g") %>%
#'     prettify_p_column() %>%
#'     pander()


test_normality <- function(x,
                           data = NULL,
                           test = "Shapiro-Wilk",
                           ...

                           , groups = NULL
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

        formula <- mosaic::mosaic_formula_q(x, groups = groups, max.slots = 3)

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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @details By default, methods \code{print.test_normality} and
#'          \code{pander.test_normality} do not print column called "method".
#' @export
#' @param rm_col_method (logical) If \code{TRUE} column "method" is not printed.
#' @rdname test_normality
# @param digits (numeric |\code{NA}) Either a number of significant digits to
# be displayed (default is 3) for p-value and test statistic or \code{NA} if
# no rounding sould be made.
print.test_normality <- function(x,
                                 ...,
                                 # digits = 3,
                                 rm_col_method = TRUE){
    method_of_test <- levels(x$method)

    # Should column "method" be removed?
    if (rm_col_method == TRUE) {
        x$method <- NULL
    }

    # # Round
    # if (!is.na(digits)) {
    #     if (is.numeric(x$statistic)) {
    #         x$statistic %<>% SIGNIF(digits) %>% as.character()
    #     }
    #     if (is.numeric(x$p.value)) {
    #         x$p.value %<>% SIGNIF(digits) %>% as.character()
    #     }
    # }

    # Pirnt the name of the method
    cat("\n", "The results of" , method_of_test, "\n\n")

    NextMethod(print, x)

}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @export
#' @rdname test_normality
#'
#' @param caption (string|\code{NULL}|\code{NA}) A caption for the table with results of a normality test. If \code{NA} — a default caption is printed (default). If \code{NULL} – no caption is printed.
#'
pander.test_normality <- function(x,
                                  ...,
                                  caption = NA,
                                  rm_col_method = TRUE) {

    method_of_test <- levels(x$method)

    # Should column "method" be removed?
    if (rm_col_method == TRUE) {
        x$method <- NULL
    }

    # Add caption
    caption <-
        if (is.null(caption)) {
            NULL
        } else if (is.na(caption)) {
            paste0("The results of " , method_of_test, ("."))
        } else {
            caption
        }

    NextMethod("pander", x, caption = caption, ...)

}
