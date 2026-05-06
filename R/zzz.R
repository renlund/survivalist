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

utils::globalVariables(c(".", ".hi", ".id", ".lo", ".maxrowid", ".rowid", "Color", "Corr", "Ctxt", "V1", "X", "a", "alias", "an_end", "b", "bad.bound", "bad.format", "begin", "capacity", "change", "cumstat", "cumstatcat", "dummy", "dummy2", "end", "error.log", "ev", "ev_ch", "event", "first.id", "gr", "gr0", "i", "i.capacity", "i.pills", "i.state", "i.usage", "i.x", "i.xmax", "i.xmin", "i.y", "i.ymax", "i.ymin", "id", "inventory", "mainHR", "mainHR.l", "mainHR.u", "mainHRinv", "mainHRinv.l", "mainHRinv.u", "match.in", "n.risk", "pills", "run", "start", "stat", "state", "store.begin", "store.end", "ti", "ti_ch", "time", "tmp", "tri", "trt", "usage", "value", "variable", "x.term", "x00", "x00date", "xdate", "y", "y.term"))
