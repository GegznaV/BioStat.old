#  test_normality() =========================================================
#' Normality Tests by Groups
#'
#' Perform tests of normality for each subset of groups separately.
#' The available tests include Shapiro-Wilk (default),
#' Lilliefors (Kolmogorov-Smirnov), Anderson_darling and other tests of normality.
#'
#' @param x Either a formula, a numeric vector or a name of a column
#'             in \code{data}. \itemize{
#'     \item If \code{x} is a formula (e.g. \code{variable ~ factor}), left-hand
#'     side provides the name of a variable to be tested. In the right-hand side
#'     there are names of factor variables to be used to create subsets.
#'     If the left-hand side is empty ((e.g. \code{~ factor}), right-hand
#'                                     side is treated as variable name to test.
#' }
#'
#' @param groups (\code{NULL}|factor|character) An alternative way to provide
#'                groups. If \code{x} is a numeric vector, \code{groups} must be
#'                a factor (or \code{NULL}). If  \code{x} is a sting,
#'                \code{groups} must also be a string (or \code{NULL}).
#'
#' @param data (data frame|\code{NULL}) Either a data frame that contains the
#'             variables mentioned in \code{x} or \code{NULL} (if the variables
#'             are in the function's environment).
#'
#' @param test (string | function) Either a string  (case insensitive, maybe
#' unambiguously abbreviated) with a name of nomality test or a
#' function, which carries out the test.\cr
#' Possible names of tests:\itemize{
#'     \item{"SW", "Shapiro-Wilk" — for Shapiro-Wilk test;}
#'     \item{"Lilliefors" — for Kolmogorov-Smirnov test wtih Lilliefor's correction;}
#'     \item{"AD", "Anderson-Darling" — for Anderson-Darling test;}
#'     \item{"CVM", "CM", "Cramer-von Mises" — for Cramer-von Mises test;}
#'     \item{"SF", "Shapiro-Francia" — for Shapiro-Francia test;}
#'     \item{"Chi-squared","Pearsons" — for Pearson's chi-squared test of normality.}
#' }
#'
#' @param signif_stars (logical) If \code{TRUE}, significance stars are printed.
#'
#' @param legend (logical) If \code{TRUE}, legend for significance stars
#'                     is printed.
#'
#' @param digits_stat (integer) number of either decimal places or significant
#'                    digits to round test statistic to.
#'
#' @param format_stat (character) Number format "f", "g"  or
#'                    "auto" (default) for test statistic. More about number
#'                    formats "f" and "g" you can find in documentation of
#'                    function \code{\link[base]{formatC}} section \code{format}.
#'
#' @param show_col_method (logical) If \code{FALSE} column "method" is not
#'                   printed. \code{FALSE} is default.
#'
#' @param caption (string|\code{NULL}|\code{NA}) A caption for the table with
#'                results of a normality test. If \code{NA} — a default caption
#'                is printed (default). If \code{NULL} – no caption is printed.
#'
#' @param p_adjust_method (\code{NULL}|string) A name of p value adjustment
#'               method for multiple comparisons. For available methods check
#'               \code{\link[stats]{p.adjust.methods}}.
#'               If \code{NULL}, no adusted p value is calculated (default).
#'               If string (e.g., \code{"holm"}), an additional column
#'               \code{p.adjust} is calculated.
#'
#' @param ... Parameters to be passed to the further methods.
#'
#'
#' @inheritParams format_p_values
#' @inheritParams stats::shapiro.test
#' @inheritParams nortest::lillie.test
#' @inheritParams nortest::pearson.test
#'
#'
#' @return  \itemize{
#'       \item Function \code{test_normality} returns a data frame with
#'             normality test results for each group.
#'       \item \code{print} and \code{pander} methods format and print the
#'       results. By default, methods \code{print.test_normality} and
#'       \code{pander.test_normality} do not print column called "method".
#' }
#'
#'
#' @export
#'
#' @importFrom stats shapiro.test
#' @import nortest
#'
#' @seealso \itemize{
#'   \item \code{\link[stats]{shapiro.test}} in package \pkg{stats};
#'   \item \code{\link[nortest]{lillie.test}} in package \pkg{nortest};
#'   \item \code{\link[nortest]{pearson.test}} in package \pkg{nortest};
#'   \item \code{\link[nortest]{ad.test}} in package \pkg{nortest};
#'   \item \code{\link[nortest]{cvm.test}} in package \pkg{nortest};
#'   \item \code{\link[nortest]{sf.test}} in package \pkg{nortest}.
#' }
#'
#' @examples
#' library(BioStat)
#' library(pander)
#' data(CO2)
#'
#' # For whole dataset
#' test_normality( ~ uptake, data = CO2)
#'
#' # For each subgroup
#' test_normality(uptake ~ Treatment, data = CO2)
#'
#' # For each subgroup by several factor variables
#' rez <- test_normality(uptake ~ Type + Treatment,
#'                       data = CO2)
#' rez
#'
#' # Modify printed output
#' print(rez, rm_zero = TRUE)
#'
#' print(rez, digits_p = 2)
#'
#'
#' # Choose another test of normality
#' rez2 <- test_normality(uptake ~ Type + Treatment,
#'                       data = CO2,
#'                       test = "chi-sq")
#' rez2
#'
#'
#' # Print as a 'pandoc' table (for RMarkdown reports)
#' pander(rez)
#'
#' pander(rez, digits_stat = 2)
#'
#' pander(rez, digits_p = 2)
#'
#' pander(rez, digits_p = 4, signif_stars = FALSE)
#'
#'
#' \donttest{\dontrun{
#' # View unformatted results in a separate window
#' View(rez)
#' }}
#'
#' # Show object's class
#' class(rez)
#'
test_normality <- function(x,
                           data = NULL,
                           test = "Shapiro-Wilk",
                           p_adjust_method = NULL,
                           ...,
                           groups = NULL
) {

    # Choose the test ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (is.function(test)) {
        use_test <- test

    } else if (checkmate::test_character(test, len = 1)) {
        # Possible choices
        available_tests = c(
            "SW", "Shapiro-Wilk",
            "Lilliefors",
            "AD", "Anderson-Darling",
            "CVM", "CM", "Cramer-von Mises",
            "SF", "Shapiro-Francia",
            "Chi-squared", "Pearsons"
        )

        test <- match.arg(tolower(test), tolower(available_tests))

        use_test <- switch(test,
                           "sw" = ,
                           "shapiro.test" = ,
                           "shapiro-wilk"     = stats::shapiro.test,

                           "lillie.test" = ,
                           "lilliefors"       = nortest::lillie.test,

                           "ad" = ,
                           "ad.test" = ,
                           "anderson-darling" = nortest::ad.test,

                           "cvm" = ,
                           "cm"  = ,
                           "cvm.test" = ,
                           "cramer-von mises" = nortest::cvm.test,

                           "sf" = ,
                           "sf.test" = ,
                           "shapiro-francia"  = nortest::sf.test,

                           "chi-squared" = ,
                           "pearson.test" = ,
                           "pearsons"         = nortest::pearson.test
        )

    } else {
        stop("\n`test` must be either a function",
             "\n or a name of a test (a string of length 1).")
    }

    # Output
    test_(x,
          data = data,
          p_adjust_method = p_adjust_method,
          ...,
          groups = groups,
          test = use_test)
}

# test_()===================================================================
test_ <- function(x,
                  data = NULL,
                  p_adjust_method = NULL,
                  ...,
                  groups = NULL,
                  test = stats::shapiro.test)
    # na.rm = getOption("na.rm", FALSE)
{
    # Make formula according to input type
    if (is.numeric(x)) {
        if (!is.null(groups)) {
            data <- data.frame(x = x, groups = groups)
            x <- x ~ groups
        } else {
            data <- data.frame(x = x)
            x <- ~ x
        }
    }

    if (is.null(data)) {
        data <- rlang::f_env(x)
    }

    if (rlang::is_formula(x)) {
        # Select necessary variables only
        data <- stats::model.frame(x, data = data)

        # To indicate, that there is no grouping the first column constant
        # if (ncol(data) == 1)
        #     data[["Groups"]] <- "<all values>"

        var_names <- names(data)
        gr_vars <- rlang::syms(var_names[-1])

        if (!is.numeric(data[[1]]))
            stop_glue("Variable `{var_names[1]}` must be numeric.")

        # The main test
        rez <-
            data %>%
            dplyr::group_by(!!!gr_vars)  %>%
            dplyr::do(.[[1]] %>%
                          test() %>%
                          broom::tidy()
            )  %>%
            dplyr::ungroup()  %>%
            dplyr::select(method, dplyr::everything())  %>%
            as.data.frame()

        # If adjusted p value is needed
        if (!is.null(p_adjust_method)) {
            rez$p.adjust <- p.adjust(rez$p.value, method = p_adjust_method)
        }

        rez <- structure(rez,
                         class = c("test_normality", "data.frame"),
                         test = levels(rez$method),
                         p_adjust_method = p_adjust_method)

        return(rez)

    } else {
        stop("Incorrect input")
    }

}


# print()  ----------------------------------------------------------------
#' @rdname test_normality
#' @export
print.test_normality <- function(x,
                                 ...,
                                 digits_p = 3,
                                 signif_stars = TRUE,
                                 digits_stat = 3,
                                 format_stat = c("auto","f", "g"),
                                 rm_zero = FALSE,
                                 legend = TRUE,
                                 show_col_method = FALSE
                                 ) {
    format_stat <- match.arg(format_stat)
    x <- format_object(x,
                       digits_p = digits_p,
                       digits_stat = digits_stat,
                       format_stat = format_stat,
                       signif_stars = signif_stars,
                       signif_as_separate_column = TRUE,
                       signif_stars_col_name = " ",
                       rm_zero = rm_zero,
                       show_col_method = show_col_method
    )

    # Pirnt the name of the method
    cat("\n", "The results of", which_test(x), "\n\n")

    # Print main results
    NextMethod(print, x)

    # Print signif. stars legend
    if (legend == TRUE && signif_stars == TRUE)
        cat("\nLegend for p-values:  \n", signif_stars_legend(), "\n")

    # Print p adjust. method:
    p_adjust_method <- attr(x, "p_adjust_method")
    if (!is.null(p_adjust_method) && nrow(x) > 1)
        cat("The method for p-value adjustment:",
            first_capital(p_adjust_method),
            "\n")
}
# pander()  ----------------------------------------------------------------
#' @export
#' @rdname test_normality
pander.test_normality <- function(x,
                                  caption = NA,
                                  ...,
                                  digits_p = 3,
                                  signif_stars = TRUE,
                                  digits_stat = 3,
                                  format_stat = c("auto", "f", "g"),
                                  rm_zero = FALSE,
                                  legend = TRUE,
                                  show_col_method = FALSE) {

    format_stat <- match.arg(format_stat)

    x <- format_object(x,
                       digits_p = digits_p,
                       digits_stat = digits_stat,
                       format_stat = format_stat,
                       signif_stars = signif_stars,
                       signif_as_separate_column = FALSE,
                       rm_zero = rm_zero,
                       show_col_method = show_col_method
    )

    # String for p adjust. method:
    p_adjust_method <- attr(x, "p_adjust_method")
    adj_sting <-
        if (!is.null(p_adjust_method) & nrow(x) > 1) {
            paste0(" The method for p-value adjustment: ",
                   first_capital(p_adjust_method), ".")
        } else {
            ""
        }

    # Add caption
    caption <-
        if (is.null(caption)) {
            NULL
        } else if (is.na(caption)) {
            glue::glue('The results of {which_test(x)}.{adj_sting}')
        } else {
            caption
        }

    # Print table of results
    NextMethod("pander", x, caption = caption, ...)

    # Print legend
    if (legend == TRUE && signif_stars == TRUE)
        cat("Legend for p-values:  \n`", signif_stars_legend(), "`  \n")

}

# helpers --------------------------------------------------------------------
which_test <- function(x) {
    attr(x, "test")
}
