levelled_factor <- function(x){
    if(!is.factor(x)){
        ## message("input is not a factor")
        u <- sort(unique(x[!is.na(x)]))
        if(length(u) > 0) factor(u, levels = u) else factor(character(0))
    } else {
        l <- levels(x)
        factor(l, levels = l)
    }
}

##' Expand a data.frame through factors
##'
##' Fill in missing factor combinations within a data frame. Typical use case is
##' for filling in a grouped calculation where certain factor combinations are
##' missing due to there being no data, see the examples given.
##' @param data a data.frame
##' @param factors character vector, name of factors in data
##' @return a data.frame
##' @examples
##' d <- data.table(gr = factor(LETTERS[c(2,1,1,2,2)],
##'                             levels = LETTERS[1:3]),
##'                 foo = factor("b", levels = letters[1:2]),
##'                 x = c(0,5,6,1,2))
##' r <- d[, .(n = .N, mean = mean(x)), by = .(gr, foo)]
##' r ## <--  not all factor combinations are present
##' expand_df(c("gr", "foo"), data = r)
##' @export
expand_df <- function(data, factors){
    L <- as.list(NULL)
    for(i in seq_along(factors)){
        L[[i]] <- levelled_factor(data[[factors[i]]])
    }
    D <- as.data.table(data)
    EG <- as.data.table(expand.grid(L))
    setnames(EG, new = factors)
    R <- merge(D, EG, all.y = TRUE)
    if(is.data.table(data)) R else as.data.frame(R)
}
