##' statistics for time dependent covariates
##'
##' Given a time dependent data set ('data' with variables 'tstart' and 'tstop'
##' indicating the start and stop times for time intervals) with a covariate
##' ('var'), this function will calculate 'FUN' for all distinct time points
##' @param data a data.frame
##' @param var character; name of variable of interest
##' @param FUN a function. For tdc_statistic this operates on 'var' and can
##'     return a vector of values (if named, these will be retained in the
##'     output). For tdc_calculator this function takes a subset of data as the
##'     inputand should return a data.frame. See examples for clarification.
##' @param tstart character; name of 'tstart' variable
##' @param tstop character; name of 'tstop' variable
##' @examples
##' d <- data.frame(id = 1:2, tstart = 0, tstop = c(10,12))
##' u <- data.frame(id = 1:2, t = c(3,7))
##' tm <- survival::tmerge(d, d, id = id, tstart = tstart, tstop = tstop)
##' data <- survival::tmerge(tm, u, id = id, U = tdc(t))
##' tdc_statistic(data, var = "U", FUN = mean)
##' tdc_statistic(data, var = "U", FUN = function(x){
##'     c("min" = min(x), "mean" = mean(x), "max" = max(x))
##' })
##' tdc_count(data)
##' ## for more advanced calculations one needs tdc_calculator
##' d <- data.frame(id = 1:5, tstart = 0, tstop = c(10:14),
##'                 gr = rep(LETTERS[1:2], c(2,3)))
##' u <- data.frame(id = 1:4, t = c(3,15,7,10))
##' tm <- survival::tmerge(d, d, id = id, tstart = tstart, tstop = tstop)
##' data <- survival::tmerge(tm, u, id = id, U = tdc(t))
##' groupFnc <- function(d){
##'   fnc <- function(SD){
##'     with(SD, data.frame("min" = min(U), "mean" = mean(U), "max" = max(U)))
##'   }
##'   L <- lapply(X = split(d, f = d$gr), FUN = fnc)
##'   cbind(gr = rep(names(L), lapply(L, nrow)), do.call(rbind, L))
##' }
##' tdc_calculator(data, FUN = groupFnc)
##' ## of course, a function like groupFnc is much easier to define using data.table
##' groupFnc2 <- function(d){
##'   D <- data.table::as.data.table(d)
##'   D[, .("min" = min(U), "mean" = mean(U), "max" = max(U)), by = gr]
##' }
##' tdc_calculator(data, FUN = groupFnc2)
##' @export
tdc_statistic <- function(data, var, FUN, tstart = "tstart", tstop = "tstop"){
    ## check arguments:
    properties(data, class = "data.frame")
    properties(var, class = "character", length = 1, na.ok = FALSE)
    properties(FUN, class = "function", length = 1)
    properties(tstart, class = "character", length = 1, na.ok = FALSE)
    properties(tstop, class = "character", length = 1, na.ok = FALSE)
    inclusion(names(data), nm = "data", include = c(var, tstart, tstop))
    D <- as.data.table(data)[, c(tstart, tstop, var), with = FALSE]
    setnames(D, new = c("tstart", "tstop", "var"))
    final <- D[, max(tstop)]
    timepoints <- setdiff(D[, sort(unique(c(tstart, tstop)))], final)
    metaFnc <- function(t, Data = D){
        Data[tstart <= t & tstop > t, lapply(FUN(var), FUN = identity)]
        ## Note: the seemingly weird construction with lapply here is so that
        ## metaFnc returns a sane data.table in the case where FUN returns a
        ## vector
    }
    R <- cbind(t = timepoints, rbindlist(lapply(X = timepoints, FUN = metaFnc)))
    return_dt <- return_data.table(is.data.table(data))
    if(return_dt) R else as.data.frame(R)
}

##' @describeIn tdc_statistic tdc_calculator: similar to tdc_statistic but
##'     requires a function that operates on the data.frame. This also allows
##'     for grouped calculations.
##' @export
tdc_calculator <- function(data, FUN, tstart = "tstart", tstop = "tstop"){
    ## check arguments:
    properties(data, class = "data.frame")
    properties(FUN, class = "function", length = 1)
    properties(tstart, class = "character", length = 1, na.ok = FALSE)
    properties(tstop, class = "character", length = 1, na.ok = FALSE)
    inclusion(names(data), nm = "data", include = c(tstart, tstop))
    D <- as.data.table(data)
    final <- D[, max(b), env = list(b = tstop)]
    timepoints <- setdiff(
        x = D[, sort(unique(c(a, b))), env = list(a = tstart, b = tstop)],
        y = final
    )
    metaFnc <- function(t, Data = D){
        Data[a <= t & b > t, cbind(t = t, FUN(.SD)),
             env = list(a = tstart, b = tstop)]
    }
    R <- rbindlist(lapply(X = timepoints, FUN = metaFnc))
    return_dt <- return_data.table(is.data.table(data))
    if(return_dt) R else as.data.frame(R)
}

##' @describeIn tdc_statistic Count individuals in study over time
##' @export
tdc_count <- function(data, tstart = "tstart", tstop = "tstop"){
    r <- tdc_statistic(data, var = tstart, FUN = length,
                       tstart = tstart, tstop = tstop)
    ## names(r) <- c("t", "count")
    setnames(r, new = c("t", "count"))
    rbind(r, data.frame(t = max(data[[tstop]]), count = 0))
}
