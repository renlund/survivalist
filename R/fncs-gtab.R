##' group table (gtab) functions
##'
##' @param term character; name of variable in data
##' @param data a data.frame
##' @param all logical; use all rows as one of the groups
##' @param all.first logical; put 'all' first
##' @param all.nm character; name of 'all' column
##' @param rev logical; reverse order of the levels of 'term
##' @param gtab data.frame; a "grouping table" (gtab)
##' @param n optional; number of rows wanted
##' @name gtab-fncs
NULL

##' @rdname gtab-fncs
##' @details create_gtab: create grouping table from a term in data
##' @export
create_gtab <- function(term, data, all = FALSE, all.first = FALSE,
                        all.nm = "All", rev = FALSE){
    properties(term, class = "character", length = 1, na.ok = FALSE)
    properties(data, class = "data.frame")
    properties(all, class = "logical", length = 1, na.ok = FALSE)
    properties(all.first, class = "logical", length = 1, na.ok = FALSE)
    properties(all.nm, class = "character", length = 1, na.ok = FALSE)
    properties(rev, class = "logical", length = 1, na.ok = FALSE)
    inclusion(names(data), nm = "names of data", include = term)
    y <- data[[term]]
    properties(y, class = c("character", "factor"))
    L <- if(is.factor(y)) levels(y) else sort(unique(y[!is.na(y)]))
    if(rev) L <- rev(L)
    all.nm <- rename(all.nm, avoid = L)
    n <- nrow(data)
    R <- as.list(NULL)
    if(all & all.first) R[[all.nm]] <- rep(TRUE, n)
    for(i in seq_along(L)){
        R[[L[i]]] <- !is.na(y) & y == L[i]
    }
    if(all & !all.first) R[[all.nm]] <- rep(TRUE, n)
    as.data.frame(R)
}

##' @rdname gtab-fncs
##' @details check_gtab: check a grouping table
##' @export
check_gtab <- function(gtab, n = NULL){
    properties(gtab, class = "data.frame")
    if(!is.null(n)){
        properties(n, class = c("integer", "numeric"),
                   length = 1, na.ok = FALSE)
        if(nrow(gtab) != n){
            s <- paste0("gtab need to have ", n, " rows.")
            stop(s)
        }
    }
    l <- unlist(lapply(gtab, function(z) class(z)[1] == "logical"))
    if(any(!l)){
        s <- paste0("gtab contains non-logical columns (",
                    paste0(names(l)[!l], collapse= ", "),
                    ")")
        stop(s)
    }
    m <- unlist(lapply(gtab, function(z) any(is.na(z))))
    if(any(m)){
        s <- paste0("gtab has columns (",
                    paste0(names(m)[m], collapse = ", "),
                    ") with missing values")
        stop(s)
    }
    gtab
}

##' @rdname gtab-fncs
##' @details gtab_equiv2factor: test if grouping table could be defined by a
##'     single factor
##' @export
gtab_equiv2factor <- function(gtab, verbose = TRUE, check = FALSE){
    if(check) check_gtab(gtab)
    rs <- rowSums(gtab)
    if( all(!is.na(rs)) && all(rs == 1)){
        TRUE
    } else {
        if(verbose){
            if(any( is.na(rs) )) message("missing values exists")
            if(any( rs > 1, na.rm = TRUE )) message("overlapping groups")
            if(any( rs == 0, na.rm = TRUE )){
                message("some individuals belongs to no group")
            }
        }
        FALSE
    }
}

##' @rdname gtab-fncs
##' @details gtab2factor: create a factor from a grouping table (if possible)
##' @export
gtab2factor <- function(gtab, verbose = FALSE, check = FALSE){
    if(check) check_gtab(gtab)
    equiv <- gtab_equiv2factor(gtab, verbose = verbose, check = FALSE)
    if(equiv){
        L <- names(gtab)
        N <- length(gtab)
        n <- nrow(gtab)
        x <- character(n)
        for(i in seq_along(gtab)){
            x[which(gtab[[i]])] <- L[i]
        }
        factor(x, levels = L)
    } else {
        warning("gtab not equivalent to a factor")
        factor(character(0))
    }
}

################################################################################
##         SANITY CHECKS AND TESTS                                            ##
################################################################################

if(FALSE){

    d <- data.frame(x = c(1,1,2), y = letters[c(1,1,2)])
    create_gtab(term = "y", data = d)
    create_gtab(term = "y", data = d, all = TRUE, all.first = FALSE)
    create_gtab(term = "y", data = d, all = TRUE, all.first = TRUE)
    create_gtab(term = "y", data = d, all = TRUE, all.first = TRUE, rev = TRUE)

    gl <- data.frame(a = c(T,T,F), b = c(F,F,T))
    gtab_equiv2factor(gl)
    gtab2factor(gl)

    gl <- data.frame(a = c(T,T,F,F,F), b = c(T,F,T,NA,F))
    gtab_equiv2factor(gl)

}
