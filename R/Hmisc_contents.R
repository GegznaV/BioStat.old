# ==============================================================================
contents <- function(object, ...) {
    UseMethod('contents')
}
# ------------------------------------------------------------------------------
contents.data.frame <- function(object, sortlevels=FALSE,
                                id=NULL, range=NULL, values=NULL, ...)
{
    dfname <- deparse(substitute(object))
    nam <- names(object)
    d <- dim(object)
    n <- length(nam)
    fl <- nas <- integer(n)
    cl <- sm <- lab <- un <- longlab <- character(n)
    Lev <- list()
    for(i in 1:n) {
        x <- object[[i]]
        at <- attributes(x)
        if (length(at$label))
            lab[i]     <- at$label
        if (length(at$longlabel))
            longlab[i] <- at$longlabel

        if (length(at$units))
            un[i] <- at$units

        atl <- at$levels
        fl[i] <- length(atl)
        cli <- at$class[at$class %nin% c('labelled', 'factor')]
        if(length(cli)) cl[i] <- cli[1]

        sm[i] <- storage.mode(x)
        nas[i] <- sum(is.na(x))
        if (length(atl)) {
            if (sortlevels)
                atl <- sort(atl)
            if (length(Lev))
                for (j in 1:length(Lev)) {
                    w <- Lev[[j]]
                    if (!is.name(w) && is.logical(all.equal(w, atl))) {
                        atl <- as.name(names(Lev)[j])
                        break
                    }
                }
            Lev[[nam[i]]] <- atl
        }
    }

    w <- list(Labels  = if (any(lab != '')) lab,
              Units   = if (any(un != ''))  un,
              Levels  = if (any(fl > 0))    fl,
              Class   = if (any(cl != ''))  cl,
              Storage =                     sm,
              NAs     = if (any(nas > 0))   nas )

    w <- w[sapply(w, function(x)length(x) > 0)]

    ## R does not remove NULL elements from a list
    structure(list(contents=data.frame(w, row.names=nam),
                   dim=d, maxnas=max(nas),
                   id=id, rangevar=range, valuesvar=values,
                   unique.ids = if(length(id) && id %in% nam)
                       length(unique(object[[id]])),
                   range = if(length(range) && range %in% nam)
                       paste(as.character(range(object[[range]], na.rm=TRUE)),
                             collapse='-'),
                   values = if(length(values) && values %in% nam)
                       paste(if(is.factor(object[[values]])) levels(object[[values]])
                             else sort(unique(object[[values]])), collapse=' '),
                   dfname=dfname,
                   Levels=Lev,
                   longLabels=if(any(longlab != ''))
                       structure(longlab, names=nam)),
              class='contents.data.frame')
}

# ------------------------------------------------------------------------------
print.contents.data.frame <-
    function(x, sort=c('none','names','labels','NAs'),
             prlevels=TRUE, maxlevels=Inf, number=FALSE, ...)
    {
        sort <- match.arg(sort)
        d <- x$dim
        maxnas <- x$maxnas
        cat('\nData frame:', x$dfname, '\t', d[1],' observations and ', d[2],
            ' variables    Maximum # NAs:', maxnas, '\n', sep='')
        if(length(x$id)) cat('Distinct ', x$id, ':', x$unique.ids, '\t', sep='')
        if(length(x$rangevar)) cat(x$rangevar, ' range:', x$range, '\t', sep='')
        if(length(x$valuesvar))cat(x$valuesvar, ':', x$values, sep='')
        cat('\n\n')
        cont <- x$contents
        nam <- row.names(cont)
        if(number) row.names(cont) <- paste(format(1:d[2]), row.names(cont))

        switch(sort,
               names={
                   cont <- cont[order(nam),,drop=FALSE]
               },
               labels={
                   if(length(cont$Labels))
                       cont <-  cont[order(cont$Labels, nam),, drop=FALSE]
               },
               NAs={
                   if(maxnas > 0)
                       cont <- cont[order(cont$NAs, nam),, drop=FALSE]
               })

        if(length(cont$Levels))
            cont$Levels <- ifelse(cont$Levels == 0, '', format(cont$Levels))

        print(cont)

        if(prlevels && length(L <- x$Levels)) {
            cat('\n')
            nam <- names(L)
            w <- .Options$width - max(nchar(nam)) - 5
            reusingLevels <- sapply(L, is.name)
            fullLevels    <- which(! reusingLevels)
            namf <- lin <- names(L[fullLevels])
            ## separate multiple lines per var with \n for print.char.matrix
            j <- 0
            for(i in fullLevels) {
                j <- j + 1
                varsUsingSame <- NULL
                if(sum(reusingLevels)) {
                    for(k in which(reusingLevels))
                        if(L[[k]] == namf[j]) varsUsingSame <- c(varsUsingSame, nam[k])
                        if(length(varsUsingSame))
                            namf[j] <- paste(c(namf[j], varsUsingSame), collapse='\n')
                }
                Li <- L[[i]]
                if(length(Li) > maxlevels) Li <- c(Li[1 : maxlevels], '...')
                lin[j] <- paste(pasteFit(Li, width=w), collapse='\n')
            }
            z <- cbind(Variable=namf, Levels=lin)
            print.char.matrix(z, col.txt.align='left', col.name.align='left',
                              row.names=TRUE, col.names=TRUE)
        }

        longlab <- x$longLabels
        if(length(longlab)) {
            if(existsFunction('strwrap'))
                for(i in 1:length(longlab)) {
                    if(longlab[i] != '')
                        longlab[i] <- paste(strwrap(longlab[i],width=.85*.Options$width ),
                                            collapse='\n')
                }
            i <- longlab != ''
            nam <- names(longlab)
            z <- cbind(Variable=nam[i], 'Long Label'=longlab[i])
            print.char.matrix(z, col.names=TRUE, row.names=FALSE,
                              cell.align='left')
        }

        invisible()
    }

# ------------------------------------------------------------------------------
html.contents.data.frame <- function(object,
                                     sort = c('none', 'names', 'labels', 'NAs'),
                                     prlevels = TRUE,
                                     maxlevels = Inf,
                                     levelType = c('list', 'table'),
                                     number = FALSE,
                                     nshow = TRUE,
                                     ...)
{

    sort <- match.arg(sort)
    levelType <- match.arg(levelType)
    mu <- markupSpecs$html
    lspace <- mu$lspace
    hrule  <- mu$hrule

    d      <- object$dim
    maxnas <- object$maxnas

    if (nshow) {
        R <- paste0(hrule,
                    '<h4>Data frame:', object$dfname, '</h4>',
                    d[1], ' observations and ', d[2],
                    ' variables, maximum # NAs:', maxnas, lspace, lspace)

        if (length(object$id))
            R <- paste0(R, 'Distinct ', object$id, ':', object$unique.ids,
                        lspace, lspace)
        if (length(object$rangevar))
            R <- paste0(R,
                        object$rangevar, ' range:', object$range,
                        lspace, lspace)
        if (length(object$valuesvar))
            R <- paste0(R,
                        object$valuesvar, ':', object$values,
                        lspace, lspace)
        R <- c(R, hrule)

    } else
        R <- paste0(hrule,
                    '<h4>Data frame:', object$dfname,'</h4>',
                    ' Variables:', d[2],
                    hrule)

    cont <- object$contents
    nam <- row.names(cont)
    if (number) {
        rn <- paste(format(1:d[2]), row.names(cont))
        nbsp <- htmlSpecial('nbsp')
        rn <- sedit(rn, ' ', paste0(nbsp, nbsp))
        row.names(cont) <- rn
    }

    switch(sort,
           names = {
               cont <- cont[order(nam), , drop = FALSE]
           },
           labels = {
               if (length(cont$Labels))
                   cont <- cont[order(cont$Labels, nam), , drop = FALSE]
           },
           NAs = {
               if (maxnas > 0)
                   cont <- cont[order(cont$NAs, nam), , drop = FALSE]
           })

    link <- matrix('',
                   nrow = nrow(cont),
                   ncol = 1 + ncol(cont),
                   dimnames = list(dimnames(cont)[[1]],
                                   c('Name', dimnames(cont)[[2]]))
    )

    longlab <- object$longLabels
    if (length(longlab)) {
        longlab <- longlab[longlab != '']
        link[names(longlab), 'Name'] <-
            paste('#longlab', names(longlab), sep = '.')
    }

    L <- object$Levels
    Lnames <- names(L)
    if (length(cont$Levels)) {
        cont$Levels <- ifelse(cont$Levels == 0,  '', format(cont$Levels))
        namUsed     <- sapply(L, function(z) if (is.name(z)) as.character(z) else '')
        reusingLevels <- namUsed != ''
        fullLevels  <- which(!reusingLevels)
        namUsed     <- ifelse(reusingLevels, namUsed, Lnames)
        names(namUsed) <- Lnames
        link[,'Levels'] <- ifelse(cont$Levels == '',
                                  '',
                                  paste('#levels', namUsed[nam], sep = '.'))
    }

    adj <- rep('l', length(cont))
    adj[names(cont) %in% c('NAs','Levels')] <- 'r'

    if (!nshow) {
        cont$NAs <- NULL
        link <- link[, colnames(link) != 'NAs', drop = FALSE]
        adj <- adj[names(adj) != 'NAs']
    }

    out <- html(cont,
                file = FALSE,
                rownames = TRUE,
                link = link,
                border = 2,
                col.just = adj,
                ...
    )
    R <- c(R, as.character(out), hrule)

    if (prlevels && length(L) > 0) {
        if (levelType == 'list') {
            R <- c(R, '<h5>Category Levels</h5>')
            for (i in fullLevels) {
                l <- L[[i]]
                nami <- Lnames[i]
                w <- nami
                if (sum(reusingLevels)) {
                    for (k in which(reusingLevels)) {
                        if (L[[k]] == nami)
                            w <- c(w, Lnames[k])
                    }
                }

                R <- c(R,
                       paste0('<a name="levels.', nami, '">',
                              '<h6>', paste(w, collapse = ', '), '</h6>'))

                if (length(l) > maxlevels)
                    l <- c(l[1:maxlevels], '...')

                for (k in l)
                    R <- c(R,  paste0('<li>', k, '</li>\n'))
            }
        }
        else {
            ## Function to split a character vector x as evenly as
            ## possible into n elements, pasting multiple elements
            ## together when needed
            evenSplit <- function(x, n) {
                indent <- function(z) {
                    if (length(z) == 1) {
                        z
                    } else {
                        c(z[1], paste0('&emsp;', z[-1]))
                    }
                }
                m <- length(x)

                if (m <= n) {
                    return(c(indent(x), rep('', n - m)))
                }

                totalLength <- sum(nchar(x)) + (m - 1)*3.5
                ## add indent, comma, space
                lineLength  <- ceiling(totalLength / n)
                y <- pasteFit(x, sep = ', ', width = lineLength)
                m <- length(y)
                if (m > n) {
                    for (j in 1:10) {
                        lineLength <- round(lineLength * 1.1)
                        y <- pasteFit(x, sep = ', ', width = lineLength)
                        m <- length(y)

                        if (m <= n)
                            break
                    }
                }
                ## Take evasive action if needed
                if (m == n) {
                    indent(y)
                } else if (m < n) {
                    c(indent(y), rep('', n - m))
                } else{
                    c(paste(x, collapse = ', '), rep('', n - 1))
                }
            }
            nam <- names(L)
            v <- lab <- lev <- character(0)
            j <- 0

            for (i in fullLevels) {
                j <- j + 1
                l <- L[[i]]
                if (length(l) > maxlevels)
                    l <- c(l[1:maxlevels], '...')

                nami <- nam[i]
                v <- c(v, nami)
                w <- nami

                if (sum(reusingLevels))
                    for (k in which(reusingLevels))
                        if (L[[k]] == nam[i])
                            w <- c(w, nam[k])

                lab <- c(lab, evenSplit(w, length(l)))
                lev <- c(lev, l)
            }

            z <- cbind(Variable = lab, Levels = lev)

            out <- html(z,
                        file = FALSE,
                        link = ifelse(lab == '', '', paste('levels', v, sep =
                                                               '.')),
                        linkCol = 'Variable',
                        linkType = 'name',
                        border = 2,
                        ...
            )
            R <- c(R, as.character(out), hrule)
        }
    }

    i <- longlab != ''

    if (any(i)) {
        nam <- names(longlab)[i]
        names(longlab) <- NULL
        lab <- paste('longlab', nam, sep = '.')
        z <- cbind(Variable = nam, 'Long Label' = longlab[i])
        out <- html(z,
                    file = FALSE,
                    link = lab,
                    linkCol = 'Variable',
                    linkType = 'name',
                    ...
        )
        R <- c(R, as.character(out), hrule)
    }
    htmltools::HTML(paste0(R, '\n'))
}

# ------------------------------------------------------------------------------
contents.list <- function(object, dslabels = NULL, ...) {
    nam <- names(object)

    if (length(dslabels)) {
        dslabels <- dslabels[nam]
        names(dslabels) <- NULL
    }

    g <- function(w) {
        if (length(w) == 0 || is.null(w)) {
            c(Obs = 0,
              Var = if (is.null(w)) NA else length(w), Var.NA = NA)
        } else {
            c(Obs = length(w[[1]]),
              Var = length(w),
              Var.NA = sum(sapply(w,
                                  function(x) {sum(is.present(x)) == 0})))
        }
    }

    v <- t(sapply(object, g))


    structure(list(contents = if (length(dslabels)) {
        data.frame(
            Label = dslabels,
            Obs = v[, 'Obs'],
            Var = v[, 'Var'],
            Var.NA = v[, 'Var.NA'],
            row.names = nam
        )
    } else {
        data.frame(
            Obs = v[, 'Obs'],
            Var = v[, 'Var'],
            Var.NA = v[, 'Var.NA'],
            row.names = nam
        )
    }),
    class = 'contents.list')
}

# ------------------------------------------------------------------------------
print.contents.list <- function(x,
                                sort = c('none', 'names', 'labels', 'NAs', 'vars'),
                                ...)
{
    sort <- match.arg(sort)
    cont <- x$contents
    nam <- row.names(cont)

    ord <- switch(sort,
                  none = 1:length(nam),
                  names = order(nam),
                  vars = order(cont$Var),
                  labels = order(cont$Label, nam),
                  NAs = order(cont$Var.NA, nam)
    )

    cont <- cont[ord, ]

    print(cont)
    invisible()
}
# ==============================================================================