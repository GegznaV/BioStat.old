# =============================================================================
#' Normality tests by goups
#'
#' Perform Shapiro-Wilk (default),
#' Lilliefors (Kolmogorov-Smirnov), Anderson_darling and other tests of normality.
#'
#' @param x Either a formula, a numeric vector or a name of a column
#'          in \code{data}. \itemize{
#'     \item If \code{x} is a formula (e.g. \code{variable ~ factor}), left-hand
#'          side provides name of variable to be tested. In the right-hand side
#'          there are names of factor variables to be used to create subsets.
#'          If the left-hand side is empty ((e.g. \code{~ factor}), right-hand
#'          side is treated as variable name to test.
#'          }
#'
#' @param data (data rame|\code{NULL}) Either a data frame that contains the
#'             variables mentioned in \code{x} or \code{NULL} (if the variables
#'             are in the function's environment).
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
#'
#' @param method (string) p value adjustment method for multiple comparisons.
#'        For available methods check \code{\link[base]{p.adjust.methods}}.
#'
#' @param ... Parameters to be passed to the main function
#'            (defined/selected in \code{test}) that carries out a normality test.
#'
#'
#' @inheritParams format_p_values
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
#' test_normality(uptake ~ Treatment, data = CO2)
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
#'
#' class(rez2)
#'
#'
#' # Print as a 'pandoc' table
#' pander(rez)
#'
#'
#' pander(rez, digits = 3)
test_normality <- function(x,
                           data = NULL,
                           test = "Shapiro-Wilk",
                           method = NULL,
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
          method = method,
          ...,
          groups = groups,
          test = use_test)

}

# =============================================================================
test_ <- function(x,
                  data = NULL,
                  method = NULL,
                  ...,
                  groups = NULL,
                  test = stats::shapiro.test)
    # na.rm = getOption("na.rm", FALSE)
{
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
                          test(...) %>%
                          broom::tidy()
            )  %>%
            dplyr::ungroup()  %>%
            dplyr::select(method, dplyr::everything())  %>%
            as.data.frame()

        # If adjusted p value is needed
        if (!is.null(method)) {
            rez$p.adjust <- p.adjust(rez$p.value, method = method)
        }

        rez <- structure(rez,
                         class = c("test_normality", "data.frame"),
                         test = levels(rez$method),
                         p_adjust_method = method)

        return(rez)

    } else {
        stop("Incorrect input")
    }

}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname test_normality
#' @details By default, methods \code{print.test_normality} and
#'          \code{pander.test_normality} do not print column called "method".
#' @export
#' @param signif_stars (logical) If \code{TRUE}, significance stars are pronted.
#' @param digits_stat (integer) number of either decimal places or significant digits to round test statistic to.
#' @param show_col_method (logical) If \code{FALSE} column "method" is not printed.
print.test_normality <- function(x,
                                 ...,
                                 digits_p = 2,
                                 signif_stars = TRUE,
                                 digits_stat = 3,
                                 rm_zero = FALSE,
                                 show_col_method = FALSE
                                 ) {

    x <- format_object(x,
                       digits_p = digits_p,
                       digits_stat = digits_stat,
                       signif_stars = signif_stars,
                       signif_as_separate_column = TRUE,
                       signif_stars_col_name = " ",
                       rm_zero = rm_zero,
                       show_col_method = show_col_method
    )


    # Pirnt the name of the method
    cat("\n", "The results of" , which_test(x), "\n\n")

    NextMethod(print, x)

    signif_stars_legend()
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @export
#' @rdname test_normality
#'
#' @param caption (string|\code{NULL}|\code{NA}) A caption for the table with results of a normality test. If \code{NA} — a default caption is printed (default). If \code{NULL} – no caption is printed.
#'
pander.test_normality <- function(x,
                                  caption = NA,
                                  ...,
                                  digits_p = 2,
                                  signif_stars = TRUE,
                                  digits_stat = 3,
                                  rm_zero = FALSE,
                                  show_col_method = FALSE) {

    x <- format_object(x,
                       digits_p = digits_p,
                       digits_stat = digits_stat,
                       signif_stars = signif_stars,
                       signif_as_separate_column = FALSE,
                       rm_zero = rm_zero,
                       show_col_method = show_col_method
    )

    # Add caption
    caption <-
        if (is.null(caption)) {
            NULL
        } else if (is.na(caption)) {
            glue::glue('The results of {which_test(x)}.')
        } else {
            caption
        }

    NextMethod("pander", x, caption = caption, ...)

}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
which_test <- function(x) {
    attr(x, "test")
}
# ============================================================================
format_object <- function(x, ...) {
    UseMethod("format_object")
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
format_object.test_normality <- function(x,
                                         digits_p = 3,
                                         digits_stat = 3,
                                         signif_stars = TRUE,
                                         signif_as_separate_column = TRUE,
                                         signif_stars_col_name = "signif.",
                                         rm_zero = FALSE,
                                         show_col_method = FALSE
) {
    # Should column "method" be removed?
    if (show_col_method == FALSE) {
        x$method <- NULL
    }
    # Add column with significance stars
    if (signif_stars == TRUE & signif_as_separate_column == TRUE) {
        x[[signif_stars_col_name]] <- sprintf("%-3s", get_signif_stars(x$p.value))
        # There is no point to show significance stars two times
        signif_stars = FALSE
    }

    # Round
    x <- format_as_p_columns(x,
                             digits_p = digits_p,
                             rm_zero = rm_zero,
                             signif_stars = signif_stars)

    if (is.numeric(x$statistic)) {
        format <- if (all(x$statistic < 1)) "f" else "g"

        x$statistic %<>%
            formatC(format = format, digits = digits_stat)
    }

    if (rm_zero == TRUE) {
        x$statistic %<>% BioStat::rm_zero()
    }
    # Output:
    x
}