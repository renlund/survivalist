##' cumsum bounded
##'
##' a version of cumsum that will stay within specified boundaries
##' @param x a numeric vector
##' @param low numeric; lower bound
##' @param high numeric; upper bound
##' @return a numeric vector
##' @export
##' @examples
##' cumsum_bounded(c(0,-1,-100,1,7,-99,1,1,1,-1), low = -50)
##' cumsum_bounded(c(0,-1,-100,1,7,-99,1,1,1,-1)) ## low = 0 is default
##' cumsum_bounded(c(0,-1,-100,1,7,-99,1,1,1,-1), high = 5)
cumsum_bounded <- function(x, low = 0, high = Inf){
    properties(x, class = c("numeric", "integer"), na.ok = FALSE)
    n <- length(x)
    properties(low, class = c("numeric", "integer"), length = c(1,n), na.ok = FALSE)
    properties(high, class = c("numeric", "integer"), length = c(1,n), na.ok = FALSE)
    if(any(low >= high)) stop("args do not make sense (low must be < high)")
    R <- rep(NA, n)
    if(length(low) != n) low <- rep(low, n)
    if(length(high) != n) high <- rep(high, n)
    bound <- function(z, i) min( max(z, low[i]), high[i] )
    R[1] <- bound(x[1], 1)
    if(n > 1){
        for(i in 2:n){
            R[i] <- bound(R[i-1] + x[i], i)
        }
    }
    R
}

##' last observation carried forward
##'
##' replace missing values with the last non-missing value
##' @param x a vector of values
##' @return a vector of same class and length as that of input x
##' @export
locf <- function(x){
    n <- length(x)
    if(n <= 1){
        x
    } else if(is.na(x[1])) {
        c(NA, locf(x[-1]))
    } else {
        x[!is.na(x)][cumsum(!is.na(x))]
    }
}

##' cluster by include next
##'
##' Given a logical variable indicating if the next line should belong to the
##' same cluster as the current line, this function creates an integer-valued
##' clustering variable.
##' @param incl.next a logical vector
##' @return an integer valued grouping vector
##' @export
cbin <- function (incl.next){
    properties(incl.next, class = "logical")
    n <- length(incl.next)
    if(any(is.na(incl.next[-n]))){
        s <- paste0("'incl.next' should not contains missing values ",
                    "(except possible in last entry)")
        stop(s)
    }
    c(1L, 1L + cumsum(as.integer(!incl.next[-n])))
}

#-# determine if a data.table should be returned
return_data.table <- function(input.is.data.table = NULL){
    properties(input.is.data.table, class = c("NULL", "logical"),
               length = 0:1, na.ok = FALSE)
    if(is.null(input.is.data.table)){
        input.is.data.table <- FALSE
    }
    rdt <- options("survivalist.return_data.table")[[1]]
    if(!is.null(rdt)){
        if(length(rdt) != 1 || class(rdt) != "logical" || is.na(rdt)){
            s <- paste0("option 'survivalist.return_data.table' is not ",
                        "a length 1, non-missing logical and will ",
                        "be ignored")
            warning(s)
            input.is.data.table
        } else {
            rdt
        }
    } else input.is.data.table
}

##' time conversion
##'
##' Functions for moving between numeric and Date represention of time.
##' @param num numeric; numeric representation of time
##' @param date Date; calendar representation of time
##' @param ref Date or NULL; the reference point for numeric times. If NULL, the
##'     function will check if option "survivalist.ref_date" has been set.
##' @name time_conversion
NULL

##' @rdname time_conversion
##' @details time_num2date: numeric to Date representation of time
##' @export
time_num2date <- function(num, ref = NULL){
    if(is.null(ref)){
        ref <- options("survivalist.ref_date")[[1]]
    }
    properties(ref, class = "Date", length = 1, na.ok = FALSE)
    properties(num, class = c("numeric", "integer"))
    if(any(is.na(num))){
        s <- paste0("there are missing values in 'num'")
        warning(s)
    }
    ref + num
}

##' @rdname time_conversion
##' @details time_date2num: Date to numeric representation of time
##' @export
time_date2num <- function(date, ref = NULL){
    if(is.null(ref)){
        ref <- options("survivalist.ref_date")[[1]]
    }
    properties(ref, class = "Date", length = 1, na.ok = FALSE)
    properties(date, class = c("Date"))
    if(any(is.na(date))){
        s <- paste0("there are missing values in 'date'")
        warning(s)
    }
    as.numeric(date - ref)
}

##' set tstart to 0
##'
##' Reset individual tstart to begin at 0.
##' @param data data.frame
##' @param id character, name of id variable
##' @param tstart character, name of tstart variable
##' @param tstop character, name of tstop variable
##' @export
tstart2zero <- function(data, id = "id", tstart = "tstart", tstop = "tstop"){
    properties(data, class = "data.frame")
    properties(id, class = "character", length = 1, na.ok = FALSE)
    properties(tstart, class = "character", length = 1, na.ok = FALSE)
    properties(tstop, class = "character", length = 1, na.ok = FALSE)
    inclusion(names(data), nm = "data", include = c(id, tstart, tstop))
    R <- eval(substitute(
        expr = as.data.table(data)[, `:=`(c(a, b), {
            min.A = min(A)
            .(A - min.A, B - min.A)
        }), by = .(ID)],
        env = list(A = as.name(tstart),
                   B = as.name(tstop),
                   a = tstart,
                   b = tstop,
                   ID = as.name(id))
    ))
    return_dt <- return_data.table(is.data.table(data))
    if(return_dt) R else as.data.frame(R)
}

##' truncate data at first event
##'
##' Truncate a time dependent data set at first occurence of a given event (per
##' individual).
##' @param data a data.frame
##' @param id character; name of id variable
##' @param event character; name of event variable
##' @examples
##' d <- data.frame(id=1,t1=c(0,5,10,15),t2=c(5,10,15,20),foo=c(0,0,1,0))
##' first_event_truncation(d, event = "foo")
##' @export
event1trunc <- function(data, id = "id", event){
    properties(data, class = "data.frame")
    properties(event, class = "character", length = 1, na.ok = 1)
    properties(id, class = "character", length = 1, na.ok = 1)
    inclusion(names(data), nm = "data", include = c(id, event))
    ## ------------------
    dummy <- "cbin_cluster"
    if(dummy %in% names(data)){
        dummy <- renaming_key(dummy, avoid = names(data), verbose = FALSE)
    }
    R <- eval(substitute(
        expr = as.data.table(data)[
          , cbin_cluster := cbin(the_event == 0), by = the_id][
            cbin_cluster == 1][
          , cbin_cluster := NULL],
        env = list(the_event = as.name(event),
                   the_id = as.name(id),
                   cbin_cluster = as.name(dummy))
    ))
    return_dt <- return_data.table(is.data.table(data))
    if(return_dt) R else as.data.frame(R)
}
