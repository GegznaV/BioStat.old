# ------------------------------------------------------------------------------
latex.describe.single <-    function(object,
                                     title = NULL,
                                     vname,
                                     file,
                                     append = FALSE,
                                     size = 'small',
                                     tabular = TRUE,
                                     greek = TRUE,
                                     lspace = c(0, 0),
                                     ...)
{

    ct <- function(..., file, append=FALSE) {
        if(file=='') cat(...)
        else cat(..., file=file, append=append)
        invisible()
    }

    oldw <- options('width')
    options(width=if(size == 'small') 95 else 85)
    on.exit(options(oldw))

    wide <- switch(size,
                   normalsize = 73,  # was 66
                   small      = 95,  # was 73
                   scriptsize =110,  # was 93
                   73)

    Values <- object$values

    ## Put graph on its own line if length of label > 3.5 inches
    ## For normalsize there are 66 characters per 4.8 in. standard width

    z   <- latexTranslate(object$descript, '&', '\\&', greek=greek)
    ## If any math mode ($ not preceeded by \) don't put label part in bold
    des <- if(! length(grep('[^\\]\\$', z)))
        paste0('\\textbf{', z, '}')
    else {
        ## Get text before : (variable name)
        sp <- strsplit(z, ' : ')[[1]]
        vnm <- sp[1]
        rem <- paste(sp[-1], collapse=':')
        paste0('\\textbf{', vnm, '}: ', rem)
    }

    if(length(object$units))
        des <- paste0(des, '{\\smaller[1] [',
                      latexTranslate(object$units),']}')

    if(length(object$format))
        des <- paste0(des, '{\\smaller~~Format:', latexTranslate(object$format),
                      '}')

    desbas <- paste(object$descript,
                    if(length(object$units))
                        paste0(' [', object$units, ']'),
                    if(length(object$format))
                        paste0('  Format:', object$format))

    ct('\\noindent', des, sep='', file=file, append=append)
    lco <- if(length(Values)) length(Values$frequency) else 0
    if(lco > 2) {
        counts <- Values$frequency
        maxcounts <- max(counts)
        ## Scale distinct values to range from 1 : lco
        va <- Values$value
        if(! is.numeric(va)) va <- 1 : lco
        else {
            rang <- range(va)
            va <- 1 + (lco - 1) * (va - rang[1]) / diff(rang)
        }
        ## \mbox{~~~} makes \hfill work
        ct(if(nchar(desbas)/(wide / 4.8) > (4.8 - 1.5))' \\\\ \\mbox{~~~} \n',
           '\\setlength{\\unitlength}{0.001in}\\hfill',
           '\\begin{picture}(1.5,.1)(1500,0)',
           '\\linethickness{0.6pt}\n', sep='', file=file, append=TRUE)
        ## Todo: may need to label limits used since are pretty()'d versions
        for(i in 1 : lco) {
            ct('\\put(',
               round(1000 * (va[i] - 1) * 1.5 / lco),',0){\\line(0,1){',
               max(1, round(1000 * counts[i] / maxcounts * .1)), '}}\n',
               sep='', file=file, append=TRUE)
        }

        ct('\\end{picture}\n', file=file, append=TRUE)
    } else ct('\n', file=file, append=TRUE)

    sz <- ''
    if(tabular) {
        ml <- nchar(paste(object$counts, collapse='  '))
        if(ml > 90)
            tabular <- FALSE
        else if(ml > 80)
            sz <- '[2]'
    }

    ct('\n{\\smaller', sz, '\n', sep='', file=file, append=TRUE)
    if(tabular) {
        if(lspace[1] != 0)
            ct('\\vspace{', -lspace[1], 'ex}\n', sep='', file=file, append=TRUE)
        ct('\\begin{tabular}{',
           paste(rep('r',length(object$counts)),collapse=''),'}\n',
           file=file, append=TRUE)
        ct(paste(latexTranslate(names(object$counts)), collapse='&'), '\\\\\n',
           file=file, append=TRUE)
        ct(paste(latexTranslate(object$counts), collapse='&'), '\\end{tabular}\n',
           file=file, append=TRUE)
    }

    vs <- if(lspace[2] != 0) function() ct('\\vspace{', -lspace[2], 'ex}\n',
                                           sep='', file=file, append=TRUE) else function() {}
    if(file != '')
        sink(file, append=TRUE)

    verb <- 0
    if(! tabular) {
        vs()
        cat('\\begin{verbatim}\n'); verb <- 1
        print(object$counts, quote=FALSE)
    }

    R <- formatdescribeSingle(object, lang='latex', verb=verb,
                              lspace=lspace, ...)
    cat(R, sep='\n')
    cat('}\n')  ## ends \smaller
    if(file != '') sink()
    invisible()
}
