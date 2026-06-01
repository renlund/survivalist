##' compare two vectors
##'
##' compare two vectors, return TRUE if they are missing in the same places and
##' have the same values in all others
##' @param x input vector to compare to y
##' @param y input vector to compare to x
##' @param verbose logical, print summary statements
##' @return a logical vector of length 1
##' @export
compare <- function(x, y, verbose = FALSE){
    V <- verbose
    if(V) cat("Comparison summary:\n")
    R <- TRUE
    ix <- is.na(x)
    iy <- is.na(y)
    i <- ix | iy
    if(!all(ix == iy)){
        if(V) cat(" + input have differing NA:s\n")
        R <- R & FALSE
    } else {
        if(V){
            if(any(i)){
                cat(" + input have same NA:s\n")
            } else {
                cat(" + both input have no NA:s\n")
            }
        }
    }
    if(all(i)){
        if(V) cat(" + there are no values to compare\n")
    } else {
        if(all(x[!i] == y[!i])){
            if(V) cat(" + all values match\n")
        } else {
            if(V) cat(" + some values don't match\n")
            R <- R & FALSE
        }
    }
    R
}

if(FALSE){

    x <- 1:5
    y <- 1:5
    compare(x, y, verbose = TRUE)

    x <- NA
    y <- NA
    compare(x, y, verbose = TRUE)

    x <- c(1,NA)
    y <- c(NA,2)
    compare(x, y, verbose = TRUE)

    x <- c(1:3,NA,7)
    y <- c(1:3,NA,7)
    compare(x, y, verbose = TRUE)

    x <- c(1,2,NA,4)
    y <- c(1,NA,NA,4)
    compare(x, y, verbose = TRUE)

}
