##' statistics for time dependent covariates
##'
##' Given a time dependent data set ('data' with variables 'tstart' and 'tstop'
##' indicating the start and stop times for time intervals) with a covariate
##' ('var'), this function will calculate 'FUN' for all distinct time points
##' @param data a data.frame
##' @param var character; name of variable of interest
##' @param FUN a function; can return a vector of values. Note: if FUN returns a
##'     named vector, these will be retained in the output
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
    foo <- function(t, Data = D){
        Data[tstart <= t & tstop > t, lapply(FUN(var), FUN = identity)]
    }
    R <- cbind(t = timepoints, rbindlist(lapply(X = timepoints, FUN = foo)))
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
