# ==============================================================================
latex.describe <- function(object,
                           title = NULL,
                           file = paste('describe',
                                        first.word(expr = attr(object, 'descript')),
                                        'tex', sep = '.'),
                           append = FALSE,
                           size = 'small',
                           tabular = TRUE,
                           greek = TRUE,
                           spacing = 0.7,
                           lspace = c(0, 0),
                           ...)
{
    at <- attributes(object)
    ct <- function(..., file, append=FALSE) {
        if(file=='') cat(...)
        else cat(..., file=file, append=append)
        invisible()
    }

    spc <- if(spacing == 0) '' else
        paste0('\\begin{spacing}{', spacing, '}\n')
    ct(spc, file=file, append=append)
    if(length(at$dimensions)) {
        ct('\\begin{center}\\textbf{', latexTranslate(at$descript), '\\\\',
           at$dimensions[2],'Variables~~~~~',at$dimensions[1],
           '~Observations}\\end{center}\n', file=file, append=TRUE)
        if(length(at$naprint))
            ct(at$naprint,'\\\\\n', file=file, append=TRUE)

        ct('\\smallskip\\hrule\\smallskip{\\',size,'\n',
           sep='', file=file, append=TRUE)
        vnames <- at$names
        i <- 0
        for(z in object) {
            i <- i + 1
            if(length(z)==0)
                next

            val <- z$values
            potentiallyLong <-
                length(val) && ! is.matrix(val) &&
                length(val) != 10 || ! all(names(val)==
                                               c("L1","L2","L3","L4","L5","H5","H4","H3","H2","H1"))
            dovbox <- TRUE     # was ! potentiallyLong
            if(dovbox) cat('\\vbox{', file=file, append=TRUE)

            latex.describe.single(z, vname=vnames[i],
                                  file=file, append=TRUE,
                                  tabular=tabular, greek=greek,
                                  lspace=lspace, ...)
            ct('\\smallskip\\hrule\\smallskip\n', file=file, append=TRUE)
            if(dovbox) cat('}\n', file=file, append=TRUE)
        }

        if(length(mv <- at$missing.vars)) {
            ct('\\smallskip\\noindent Variables with all observations missing:\\ \\smallskip\n',
               file=file, append=TRUE)
            mv <- latexTranslate(mv)
            mv <- paste0('\\texttt{',mv,'}')
            mv <- paste(mv, collapse=', ')
            ct(mv, file=file, append=TRUE)
        }
        spc <- if(spacing == 0) '}\n' else '}\\end{spacing}\n'
        ct(spc, file=file, append=TRUE)
    }
    else {
        val <- object$values
        potentiallyLong <-
            length(val) && ! is.matrix(val) &&
            length(val) != 10 || ! all(names(val)==
                                           c("L1","L2","L3","L4","L5","H5","H4","H3","H2","H1"))
        dovbox <- TRUE   # was ! potentiallyLong
        if(dovbox) cat('\\vbox{', file=file, append=TRUE)
        latex.describe.single(object,
                              vname=first.word(expr=at$descript),
                              file=file, append=TRUE, size=size,
                              tabular=tabular, lspace=lspace, ...)
        if(dovbox) cat('}\n', file=file, append=TRUE)
        spc <- if(spacing == 0) '\n' else '\\end{spacing}\n'
        ct(spc, file=file, append=TRUE)
    }

    structure(list(file=file,  style=c('setspace','relsize')),
              class='latex')
}
