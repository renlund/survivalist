##' create indicators for state change
##'
##' For each state in 's', create indicator variables for the state
##' changes. This might be useful e.g. when setting up data for a multistate
##' model.
##' @param state character; name of variable keeping track of a changing state
##' @param by character (vector); name(s) of variable(s) to do calculation 'by'
##'     (typically the id variable)
##' @param data a data frame or similar
##' @param by.ref logical; if data is data.table then TRUE here means defining
##'     variables by reference
##' @return the input data frame with indicators added (named as the levels of s) for state
##'     changes
##' @examples
##' d <- data.frame(id=1, tstart = 0:7, tstop = 1:8,
##'                 state = factor(LETTERS[c(1,1,2,1,1,3,2,2)],
##'                                levels = LETTERS[4:1]))
##' state2event("state", id = "id", data = d)
##' @export
state2event <- function(state, by = "id", data, by.ref = FALSE){
    properties(state, class = "character", length = 1, na.ok = FALSE)
    if(!is.null(by)) properties(by, class = "character", na.ok = FALSE)
    properties(by.ref, class = "logical", length = 1, na.ok = FALSE)
    properties(data, class = "data.frame")
    inclusion(names(data), "the names of data", include = c(state, by))
    return_dt <- TRUE
    if(!is.data.table(data)){
        return_dt <- FALSE
        data <- as.data.table(data)
        if(by.ref) message("by.ref TRUE is useless unless data is data.table")
        by.ref <- FALSE
    } else {
        if(!by.ref) data <- copy(data)
    }
    if(data[,  any(is.na(dummy)), env = list(dummy = state)]){
        stop("don't want to handle missing in 'state'")
    }
    v <- data[, values(dummy), env = list(dummy = state)]
    data[, (v) := event_frame(x = dummy, val = v, bnry = TRUE),
         by = eval(as.character(by)),
         env = list(dummy = state)]
    if(by.ref){
        invisible(data)
    } else {
        if(return_dt) data[] else as.data.frame(data)
    }
}

if(FALSE){
    d1 <- data.frame(id=1, tstart = 0:7, tstop = 1:8,
                    state = factor(LETTERS[c(1,1,2,1,1,3,2,2)],
                                   levels = LETTERS[4:1]))
    d2 <- data.frame(id=2, tstart = 0:7, tstop = 1:8,
                    state = factor(LETTERS[c(2,2,2,1,1,3,3,3)],
                                   levels = LETTERS[4:1]))
    d <- rbind(d1,d2)
    state2event("state", by = "id", data = d)

}

state_indicator <- function(val, x, bnry = TRUE){
    if(bnry) as.integer(x == val) else x == val
}

change_indicator <- function(val, x, bnry = TRUE){
    si <- state_indicator(val = val, x = x, bnry = FALSE)
    lag_si <- shift(si, n = 1, fill = TRUE, type = "lag")
    if(bnry) as.integer(si & !lag_si) else si & !lag_si
}

event_indicator <- function(val, x, bnry = TRUE){
    shift(x = change_indicator(val = val, x = x, bnry = bnry),
          n = 1, fill = FALSE, type = "lead")
}

values <- function(x){
    if(is.factor(x)) levels(x) else sort(unique(x[!is.na(x)]))
}

state_frame <- function(x, val = NULL, bnry = TRUE){
    u <- if(is.null(val)) values(x) else val
    r <- as.data.table(lapply(u, FUN = state_indicator, x = x, bnry = bnry))
    setnames(r, new = u)
    r[]
}

change_frame <- function(x, val = NULL, bnry = TRUE){
    u <- if(is.null(val)) values(x) else val
    r <- as.data.table(lapply(u, FUN = change_indicator, x = x, bnry = bnry))
    setnames(r, new = u)
    r[]
}

event_frame <- function(x, val = NULL, bnry = TRUE){
    u <- if(is.null(val)) values(x) else val
    r <- as.data.table(lapply(u, FUN = event_indicator, x = x, bnry = bnry))
    setnames(r, new = u)
    r[]
}
