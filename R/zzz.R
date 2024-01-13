.onLoad <- function(libname, pkgname){
    op <- options()
    ## this does nothing as of yet
    op.survivalist <- list(
        survivalist.return.data.table = NULL,
        survivalist.prefix = TRUE,
        survivalist.fix = c("time" = "t.", "event" = "ev."),
        survivalist.surv.group.name = "Time-to-event"
    )
    toset <- !(names(op.survivalist) %in% names(op))
    if(any(toset)) options(op.survivalist[toset])
    invisible()
}
