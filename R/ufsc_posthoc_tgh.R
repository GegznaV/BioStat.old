# This function is used by the 'oneway' function for oneway analysis of
# variance in case a user requests post-hoc tests using the Tukey or
# Games-Howell methods.
#
# posthocTGH
#
# This function is imported from package \pkg{userfriendlyscience}
# (\code{\link[userfriendlyscience]{posthocTGH}})

#' Games-Howell and Tukey post-hoc tests
#'
#' Either Games-Howell or Tukey post-hoc tests for one-way analysis of
#' variance (ANOVA). The main part of the function code and descriptions were
#' imported from \code{posthocTGH()} in package \pkg{userfriendlyscience}.
#'
#'
#'
#'
#' @param y (numeric) a numeric vector.
#' @param x (factor) a vector that either is a factor or can be converted into
#' one.
#' @param method (\code{"games-howell"}|\code{"tukey"}) Which post-hoc
#'                tests to conduct. Valid values are "tukey" and "games-howell".
#' @param conf_level Confidence level of the confidence intervals.
#' @param digits The number of digits to show in the output.
#' @param p.adjust Any valid \code{\link{p.adjust}} method.
#' @param format_pvalue Whether to format the p values according to APA
#' standards (i.e. replace all values lower than .001 with '<.001'). This only
#' applies to the printing of the object, not to the way the p values are
#' stored in the object.
#' @param ... Further arguments to methods
#'
#' @return A list of three elements:
#' \item{input}{List with input arguments}
#' \item{intermediate}{List of intermediate objects.}
#' \item{output}{List with
#' two objects 'tukey' and 'games.howell', containing the outcomes for the
#' respective post-hoc tests.}
#'
#'
#' @note This function is based on a file that was once hosted at
#' \url{http://www.psych.yorku.ca/cribbie/6130/games_howell.R}, but has been removed
#' since. It was then adjusted for implementation in the
#' \pkg{userfriendlyscience} package. Jeffrey Baggett needed the
#' confidence intervals, and so emailed them, after which his updated functions
#' was used. In the meantime, it appears Aaron Schlegel
#' (\url{https://rpubs.com/aaronsc32}) independently developed a version with
#' confidence intervals and posted it on RPubs at
#' \url{https://rpubs.com/aaronsc32/games-howell-test}.
#'
#' @seealso \itemize{
#' \item{\url{http://www.gcf.dkf.unibe.ch/BCB/files/BCB_10Jan12_Alexander.pdf}}
#' \item{\url{https://rpubs.com/aaronsc32/games-howell-test}}
#' \item{\url{http://aoki2.si.gunma-u.ac.jp/R/src/tukey.R}}
#' \item{\url{https://stats.stackexchange.com/questions/83941/games-howell-post-hoc-test-in-r}}
#'
#' }
#'
#' @author Gjalt-Jorn Peters (Open University of the Netherlands) & Jeff Bagget
#' (University of Wisconsin - La Crosse)
#'
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#'
#' @keywords utilities
#' @examples
#' library(BioStat)
#'
#' ### Compute post-hoc statistics using the Games-Howell method
#' posthoc_tgh(y=ChickWeight$weight, x=ChickWeight$Diet)
#'
#' ### Compute post-hoc statistics using the Tukey method
#' posthoc_tgh(y=ChickWeight$weight, x=ChickWeight$Diet, method="tukey");
#'
#' @export
#'
#'
posthoc_tgh.default <- function(y,
                                x,
                                method = c("games-howell", "tukey"),
                                conf_level = 0.95,
                                digits = 2,
                                p.adjust = "holm",
                                format_pvalue = TRUE,
                                ...
) {
    UseMethod("posthoc_tgh")
}


#' @rdname posthoc_tgh
#' @export
posthoc_tgh.default <-
    function(y,
             x,
             method = c("games-howell", "tukey"),
             conf_level = 0.95,
             digits = 2,
             p.adjust = "holm",
             format_pvalue = TRUE,
             ...
    ) {
    ### Based on http://www.psych.yorku.ca/cribbie/6130/games_howell.R
    method <- tolower(method)

    tryCatch(
        method <- match.arg(method),
        error = function(err) {
            stop("Argument for 'method' not valid!")

        }
    )


    res <- list(input = as.list(environment()))


    res$intermediate <- list(x = factor(x[complete.cases(x, y)]),
                             y = y[complete.cases(x, y)])

    res$intermediate$n <- tapply(y, x, length)

    res$intermediate$groups <- length(res$intermediate$n)

    res$intermediate$df <-
        sum(res$intermediate$n) - res$intermediate$groups

    res$intermediate$means <- tapply(y, x, mean)

    res$intermediate$variances <- tapply(y, x, var)

    res$intermediate$names <- levels(res$intermediate$x)
    res$intermediate$pairNames <-
        combn(res$intermediate$groups, 2, function(ij) {
            paste0(rev(res$intermediate$names[ij]), collapse = "-")

        })

    res$intermediate$descriptives <- cbind(res$intermediate$n,
                                           res$intermediate$means,
                                           res$intermediate$variances)

    rownames(res$intermediate$descriptives) <-
        levels(res$intermediate$x)

    colnames(res$intermediate$descriptives) <-
        c('n', 'means', 'variances')


    ### Start on Tukey
    res$intermediate$errorVariance <-
        sum((res$intermediate$n - 1) * res$intermediate$variances) /
        res$intermediate$df

    res$intermediate$se <-
        combn(res$intermediate$groups, 2, function(ij) {
            sqrt(res$intermediate$errorVariance * sum(1 / res$intermediate$n[ij]))

        })
    res$intermediate$dmeans <-
        combn(res$intermediate$groups, 2, function(ij) {
            diff(res$intermediate$means[ij])
        })
    res$intermediate$t <-
        abs(res$intermediate$dmeans) / res$intermediate$se
    res$intermediate$p.tukey <- ptukey(
        res$intermediate$t * sqrt(2),
        res$intermediate$groups,
        res$intermediate$df,
        lower.tail = FALSE
    )

    res$intermediate$alpha <- (1 - conf_level)

    res$intermediate$qcrit <- qtukey(res$intermediate$alpha,
                                     res$intermediate$groups,
                                     res$intermediate$df,
                                     lower.tail = FALSE) / sqrt(2)

    res$intermediate$tukey.low <-
        res$intermediate$dmeans - (res$intermediate$qcrit * res$intermediate$se)

    res$intermediate$tukey.high <-
        res$intermediate$dmeans + (res$intermediate$qcrit * res$intermediate$se)

    res$output <- list()

    res$output$tukey <- data.frame(
        res$intermediate$dmeans,
        res$intermediate$tukey.low,
        res$intermediate$tukey.high,
        res$intermediate$t,
        res$intermediate$df,
        res$intermediate$p.tukey
    )
    res$output$tukey$p.tukey.adjusted <-
        p.adjust(res$intermediate$p.tukey,
                 method = p.adjust)


    rownames(res$output$tukey) <- res$intermediate$pairNames

    colnames(res$output$tukey) <-
        c('diff', 'ci.lo', 'ci.hi', 't', 'df', 'p', 'p.adjusted')


    ### Start on Games-Howell
    res$intermediate$df.corrected <-
        combn(res$intermediate$groups, 2, function(ij) {
            sum(res$intermediate$variances[ij] /
                    res$intermediate$n[ij]) ^ 2 /
                sum((res$intermediate$variances[ij] /
                         res$intermediate$n[ij]) ^ 2 /
                        (res$intermediate$n[ij] - 1)
                )

        })
    res$intermediate$se.corrected <-
        combn(res$intermediate$groups, 2, function(ij) {
            sqrt(sum(res$intermediate$variances[ij] / res$intermediate$n[ij]))

        })
    res$intermediate$t.corrected <-
        abs(res$intermediate$dmeans) / res$intermediate$se.corrected

    res$intermediate$qcrit.corrected <-
        qtukey(
            res$intermediate$alpha,
            res$intermediate$groups,
            res$intermediate$df.corrected,
            lower.tail = FALSE
        ) / sqrt(2)

    res$intermediate$gh.low <- res$intermediate$dmeans -
        res$intermediate$qcrit.corrected * res$intermediate$se.corrected
    res$intermediate$gh.high <- res$intermediate$dmeans +
        res$intermediate$qcrit.corrected * res$intermediate$se.corrected


    res$intermediate$p.gameshowell <-
        ptukey(
            res$intermediate$t.corrected * sqrt(2),
            res$intermediate$groups,
            res$intermediate$df.corrected,
            lower.tail = FALSE
        )
    res$output$games.howell <- data.frame(
        res$intermediate$dmeans,
        res$intermediate$gh.low,
        res$intermediate$gh.high,
        res$intermediate$t.corrected,
        res$intermediate$df.corrected,
        res$intermediate$p.gameshowell
    )

    res$output$games.howell$p.gameshowell.adjusted <-
        p.adjust(res$intermediate$p.gameshowell,
                 method = p.adjust)

    rownames(res$output$games.howell) <- res$intermediate$pairNames

    colnames(res$output$games.howell) <-
        c('diff', 'ci.lo', 'ci.hi', 't', 'df', 'p', 'p.adjusted')


    ### Set class and return object
    class(res) <- 'posthocTGH'

    return(res)


}



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname posthoc_tgh
#' @export
#' @param ... (further arguments to methods)

# @param x The object to print.
# @param digits The number of significant digits to print.
print.posthocTGH <- function(x,
                             digits = x$input$digits,
                             ...) {

    print(x$intermediate$descriptives, digits = digits)

    cat('\n')

    if (x$input$method == 'tukey') {
        dat <- x$output$tukey

    }
    else if (x$input$method == 'games-howell') {
        dat <- x$output$games.howell

    }
    dat[, c(6, 7)] <- lapply(dat[, c(6, 7)],
                             format_pvalue,
                             digits = digits,
                             includeP = FALSE)

    print(dat, digits = digits)

}
