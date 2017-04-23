
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as_number <- function(x) {
    x  %>% as.character() %>% as.numeric()

}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' eval text as a command
#' @param x (string) A text with command to evaluate.
#'
#' @param envir An environment to evaluate in.
#' @param ... further parameters to \code{\link[base]{eval}}.
#'
#' @keywords internal
#' @export
eval_ <-
    function(x, envir = parent.frame(), ...) {
        eval(parse(text = x), envir = envir, ...)
    }


# Convert to numertic and round to several significant digits
#
# @param x A value
# @param digits Number of signoficant digits
#
# @export
#
# @examples
SIGNIF <- function(x, digits = 3) {
    x %>%
        # readr::parse_number() %>%
        as_number() %>%
        signif(digits = digits)
}


#' Check and adjust length of vector according to the number of columns in a data frame
#' (Function for value recycling)
#'
#' @param x a vector to check and recycle
#' @param data a data frame
#'
#' @keywords internal
#' @export
adjust_vector_length <- function(x, data) {
    n_col <- length(data)
    len_x <- length(x)
    if (len_x == 1) {
        x <- rep_len(x, n_col)
    } else if (len_x != n_col) {
        stop("Length of `", substitute(x),
             "` must be either 1 or ", n_col,".")
    }
    return(x)
}



# =============================================================================


getVarValues <- function(VAR,
                         DATA,
                         CALL = match.call(definition = sys.function(sys.parent()),
                                           call = sys.call(sys.parent())),
                         env = parent.frame(2L)) {


    `%if_null%` <- function(a, b) {if (!is.null(a)) a else b}
    # Prepare data, if needed -------------------------------------------------
    if (inherits(DATA,"hyperSpec")){
        DATA <- DATA$..
    }

    # If VAR is NULL --------------------------------------------------------
    if(is.null(VAR)){return(VAR)}

    # Force evaluation of function arguments ----------------------------------
    force(env) # Get parent environment
    force(CALL)# Get call of function which parameters are going to be evaluated.

    # Look for missing arguments-----------------------------------------------
    missVar <- vector("logical",2)
    missVar[1] <- missing(VAR)
    missVar[2] <- missing(DATA)

    if (any(missVar)) {
        missVarTXT <- paste(c("VAR", "DATA")[missVar],
                            collapse = ", ")
        stop(paste("Missing arguments with no default values:", missVarTXT))
    }
    # -----------------------------------------------------------------------
    VAR_value <- NULL
    try({VAR_value <- VAR}, silent = TRUE)

    # If data is missing (i.e. is NULL) -------------------------------------
    if (is.null(DATA))
        return(VAR)

    # If DATA is provided ---------------------------------------------------
    # and ...
    VAR_length            <- VAR_value  %>% simplify2array  %>% length
    is_VAR_value_in_DATA  <- all(VAR_value %in% colnames(DATA))
    if (VAR_length == 1 & is_VAR_value_in_DATA)
        return(DATA[[VAR_value]])

    #  ------------------------------------------------------------------------
    DATA_length  <- nrow(DATA) %if_null% length(DATA)  # <<<< this line may
    #  need reviewing:
    #  length(data.frame) vs.
    #          length(matrix)
    if (VAR_length == DATA_length)
        return(VAR_value)

    #  ------------------------------------------------------------------------
    # Convert input variable names to character (without evaluation)
    VAR_name   <- CALL[[match.call()$VAR  %>% as.character]] %>% as.character
    is_VAR_name_in_DATA <- VAR_name %in% colnames(DATA)

    if (is_VAR_name_in_DATA)
        return(DATA[[VAR_name]])

    # VAR_value_in_DATA <- env[[DATA_name]][[VAR_name]]
    # VAR_value_in_DATA <- env[[DATA_name]][[,VAR_name,drop=TRUE]]
    # VAR_value %in% colnames(env[[DATA_name]]

    #  ------------------------------------------------------------------------

    warning("Lengths of arguments 'DATA' and 'VAR' do not match!!!")# <<<< this line may need reviewing:
    # Error message is not informative enough

    return(VAR_value)
}


#  %++% ------------------------------------------------------

#' @title Infix version of \code{paste0}
#'
#' @description
#'  Infix versions of \code{paste0} and \code{paste}.
#'
#' @details
#' \code{`\%++\%`} is an infix version of \code{paste0}. \cr
#' \code{`\%.+.\%`} is an infix version of \code{paste}.
#'
#' @param a,b values to be pasted with either \code{paste0(a,b)} or \code{paste}.
#'
#'
#' @export
#' @source Inspired by "Inxix functions" in
#' \href{http://adv-r.had.co.nz/Functions.html#function-arguments}{Advanced R}
#' by Hadley Wickham.
#' @seealso \code{\link[base]{paste}}, \code{\link[spAddins]{insertPaste0_Addin}}
#' @keywords internal
#' @examples
#' "a" %++% "b"
#' #> [1] "ab"
#'
#'
`%++%` <- function(a, b) {paste0(a, b)}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Row-bind data frames which are inside a list and keep the names of the list fields
#'
#' @param x A named list of data frames (all data frames must have the same structure.)
#'
#' @return A data frame with field names of input list as a first column (called '.group').
#' @export
#' @keywords internal
#' @examples
#' # Data:
#'
#' data <- list(
#'     A = read.table(text =
#' "           intercept    slope      type conf
#' qq_refline -39.55714 1.123535 quartiles 0.95"),
#'
#'     B = read.table(text =
#' "           intercept     slope      type conf
#' qq_refline  29.42192 0.7940267 quartiles 0.95")
#' )
#'
#'
#' rbind_df_in_list(data)
#'
#' # .group intercept     slope      type
#' # 1      A -39.55714 1.1235350 quartiles
#' # 2      B  29.42192 0.7940267 quartiles
#'


rbind_df_in_list <- function(x){
    if (!is.list(x)) stop("`x` must be a list.")

    DF <- x  %>%
        do.call(rbind, .)     %>%
        # dplyr::mutate(.group = rownames(.))  %>%
        tibble::rownames_to_column(var = ".group")  %>%
        # Remove numbers, added in `do.call(rbind, .)`
        tidyr::separate_(col = ".group",
                        into = ".group",
                        sep = "\\.\\d*$",
                        extra = "drop")

    # .group as 1-st column
        # dplyr::select_(.dots = c(".group", colnames(.)[1:(ncol(.) - 1)]))

    # Check id the output is correct
    if (colnames(DF)[1] != ".group")
        warning("Function `rbind_df_in_list` does not work properly. ",
                "The name of first variable in the output dataframe must be '.group'.")

    # rownames(DF) <- NULL

    DF
}
