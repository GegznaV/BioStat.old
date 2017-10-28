'[.describe' <- function(object, i, ...) {
    at <- attributes(object)
    object <- '['(unclass(object), i)
    structure(
        object,
        descript = at$descript,
        dimensions = c(at$dimensions[1], length(object)),
        class = 'describe'
    )
}