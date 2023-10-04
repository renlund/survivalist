.onLoad <- function(libname, pkgname){
    op <- options()
    ## this does nothing as of yet
    op.survivalist <- list(
        survivalist.return_data.table = NULL,
        survivalist.prefix = TRUE,
        survivalist.fix = c("time" = "t.", "event" = "ev.")
    )
    toset <- !(names(op.survivalist) %in% names(op))
    if(any(toset)) options(op.survivalist[toset])
    invisible()
}
