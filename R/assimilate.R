##' empty entries in vector
##'
##' Determine if entries in a vector is missing or blank
##' @param x input vector
##' @return logical vector of the same length as input (unless NULL is given)
##' @export
is.empty <- function(x){
    if(is.null(x)){
        warning("length zero input to is.empty")
        TRUE
    } else {
        is.na(x) | grepl("^ *$", x)
    }
}

##' collapse many vectors into one
##'
##' this functions acts like \code{paste0( , collapse = )} but element-wise for
##' multiple input vectors
##' @param ... one or more (character) vectors
##' @param sep character string with which to separate the collapsed input
##' @param ignore.blank logical; ignore missing values and empty strings when
##'     collapsing
##' @return character vector of the same length as input vectors
##' @export
assimilate <- function(..., sep = ";", ignore.blank = TRUE){
    properties(ignore.blank, class = "logical", length = 1)
    properties(sep, class = "character", length = 1)
    dots <- list(...)
    len <- lapply(dots, length)
    if(length(unique(len)) != 1){
        stop("[assimilate:] vectors provided in ... need to have the same length")
    }
    if(ignore.blank){
        foo <- function(x) ifelse(is.empty(x), "", paste0(x, sep))
    } else {
        foo <- function(x) paste0(x, sep)
    }
    L <- lapply(dots, foo)
    r <- Reduce(f = paste0, x = L)
    sub(pattern = paste0(sep, "$"), replacement = "", r)
}



if(FALSE){

    test <- c(NA_character_, "", " ", "   ",  ".", " . ", "    .")
    data.frame(test = test,
               empty = is.empty(test))

    assimilate(x = 1:2, y = 1)

    assimilate(x= 1:2,
               foo = LETTERS[1:2],
               zz = c("    ", "BAR"),
               qwert = c(-1, NA_integer_),
               froo = c(".", ""))

    assimilate(x= 1:2,
               foo = LETTERS[1:2],
               zz = c("    ", "BAR"),
               qwert = c(-1, NA_integer_),
               froo = c(".", ""), ignore.blank = FALSE)


}
