##' count rows
##'
##' count the number of rows that satisfy the condition given by the input
##' function
##' @param ... either same length vectors OR a single data.frame
##' @param FNC a function that can take one of the input vectors as input and
##'     return a logical vector of the same length
##' @return vector with a length equal to the number of input vectors
##' @examples
##' x1 <- 1:5
##' x2 <- -2:2
##' rowCount(data.frame(x1,x2), FNC = identity)
##' rowCount(x1, x2, FNC = identity)
##' rowSums(data.frame(x1,x2))
##' x1 <- c(1, NA, 2, 3)
##' x2 <- c(4, NA, 5, NA)
##' x3 <- c(6, NA, NA, NA)
##' rowCount(x1, x2, x3, FNC = is.na)
##' foo <- function(x) is.na(x) | x > 3
##' rowCount(x1, x2, x3, FNC = foo)
##' @export
rowCount <- function(..., FNC){
    l <- list(...)
    if(length(l) == 1 & is.data.frame(l[[1]])){
        l <- as.list(l[[1]])
    }
    rowSums(as.data.frame(lapply(l, \(x) FNC(x))))
}
