##' treatment periods defined by pill consumption
##'
##' Given a data frame ('data') with individuals (identified by 'id'), treatment
##' ('state') initiated (or continued) at 't' by adding a number of pills
##' ('pills') to an inventory of such, we calculate the times at which treatment
##' ends and reverts back to the 'null.state' (or changes), assuming that pills
##' are consumed by 'usage' each time unit. Note 1: if new pills (for the same
##' treatment/state) arrives before the previous pills are consumed, the new
##' pills will be added to the inventory of pills, i.e. they can be stockpiled.
##' Note 2: if a different treatement is initiated before the pills of the
##' previous treatment are consumed, the old inventory is nullified and a new
##' treatment period begins. Note 3: duplicated times (per individual) will be
##' ignored. Note 4: a treatment will be ongoing as long as there are pills,
##' i.e. if e.g. usage is 2 and the current inventory of pills is 1, the
##' treatment will be considered active one more unit of time. Note 5:
##' 'capacity' kicks in after the daily pills have been consumed, i.e. if you
##' initiate a treatment with a 100 pills but only have capacity for 10, you
##' will consume 'usage' pills, and then have 10 left in the inventory (given
##' that usage is less than 90, of course).
##' @param data a data.frame
##' @param id character; name of id variable
##' @param t character; name of time variable
##' @param state character; name of treatment variable
##' @param pills character; name of variable keeping track of pills added to
##'     (treatment specific) inventory
##' @param usage either name of variable (character) keeping track of pill
##'     usage, or the numeric value for all pill usage. If given as a name that
##'     does not exist in data, the value 1 will be assigned.
##' @param capacity either name of variable (character) keeping track of upper
##'     bound for the size of the pill inventory, or the numeric value for all
##'     pill inventory capacities. If given as a name that does not exist in
##'     data, the value Inf will be assigned.
##' @param null.state value for 'state' when treatment ends
##' @param simplify logical; if TRUE keep only minimal information enough to
##'     keep track of changes in treatment.
##' @return a data.frame
##' @import data.table
##' @examples
##' data <- data.frame(
##'     id = 1,
##'     t =     c(  0, 10, 30, 35, 40),
##'     state = c("A","B","B","C","C"),
##'     pills = c(  4, 10, 10, 20, 20),
##'     usage = c(  3,  1,  1,  2,  1)
##' )
##' ## note that initial state A will be active until t = 2
##' ## note that the 10 remaining pills from state C initiated at 35 (with a
##' ##   usage of 2 until) ending at 40 (when usage changes to 1) are added
##' ##   to the inventory so that state C can remain active until t = 70
##' pill_treatment(data)
##' @export
pill_treatment <- function(data, id = "id", t = "t", state = "state",
                           pills = "pills", usage = "usage",
                           capacity = "capacity", null.state = "", simplify = TRUE){
    ## check arguments:
    properties(data, class = "data.frame")
    properties(id, class = "character", length = 1, na.ok = FALSE)
    properties(t, class = "character", length = 1, na.ok = FALSE)
    properties(state, class = "character", length = 1, na.ok = FALSE)
    properties(pills, class = "character", length = 1, na.ok = FALSE)
    properties(usage, class = c("character", "numeric", "integer"),
               length = 1, na.ok = FALSE)
    properties(capacity, class = c("character", "numeric", "integer"),
               length = 1, na.ok = FALSE)
    properties(null.state, length = 1, na.ok = FALSE)
    properties(simplify, class = "logical", length = 1, na.ok = FALSE)

    ## return_dt <- is.data.table(data)
    return_dt <- return_data.table(is.data.table(data))
    uchar <- is.character(usage)
    if(uchar & !usage %in% names(data)){
        s <- paste0("'", usage, "' not in data. Value 1 assigned.")
        usage <- 1
        uchar <- FALSE
        warning(s)
    }
    cchar <- is.character(capacity)
    if(cchar & !capacity %in% names(data)){
        s <- paste0("'", capacity, "' not in data. Value Inf assigned.")
        capacity <- Inf
        cchar <- FALSE
        warning(s)
    }
    req_nm <- c(id, t, state, pills,
                if(uchar) usage else NULL,
                if(cchar) capacity else NULL)
    inclusion(names(data), nm = "data", include = req_nm)
    D <- as.data.table(data)[, req_nm, with = FALSE]
    if(!uchar){
        req_nm <- c(req_nm, "usage")
        D[, usage := usage]
        usage = "usage"
    }
    if(!cchar){
        req_nm <- c(req_nm, "capacity")
        D[, capacity := capacity]
        capacity = "capacity"
    }
    if(D[, any(pills < 0)]){
        s <- paste0("'pills' must be a positive variable")
        stop(s)
    }
    if(D[, any(usage <= 0)]){
        s <- paste0("'usage' must be a (strictly) positive variable")
        stop(s)
    }
    if(D[, any(capacity <= 0)]){
        s <- paste0("'capacity' must be a (strictly) positive variable")
        stop(s)
    }

    setnames(D, old = req_nm,
             new = c("id", "t", "state", "pills", "usage", "capacity"))
    setkey(D, id, t) ## this orders the data accordingly
    R <- rbindlist(l = lapply(X = split(D, f = D$id),
                              FUN = pill_treatment_calculator,
                              null.state = null.state,
                              simplify = simplify))
    setnames(R, old = c("id", "t", "state"),
             new = c(id, t, state))

    if(return_dt) R else as.data.frame(R)
}

##' @describeIn pill_treatment pill_treatment: The workhorse for pill_treatment; a rigid
##'     function that requires a data.table with variables named 'id' (which
##'     should be a constant), 't' (which the data.table should be ordered on),
##'     'state', 'pills', 'usage' and 'capacity'. Probably don't use this
##'     directly, there are no checks or warnings of any kind with this function
##' @export
pill_treatment_calculator <- function(data, null.state = "", simplify = TRUE){
    if(!is.data.table(data)) data <- as.data.table(data)
    N <- data[, t[.N] - t[1] + 1 + sum(ceiling(pills / usage))]
    y <- data.table(id = data$id[1], t = data$t[1] + 0:(N-1), pills = 0L)
    y[data, `:=`(state = i.state,
              pills = i.pills,
              usage = i.usage,
              capacity = i.capacity), on = "t"]
    y[, `:=`(state = locf(state),
             usage = locf(usage),
             capacity = locf(capacity))]
    y[, gr := cbin(c(state[1:(N-1)] == state[2:N], TRUE))]
    y[, inventory := cumsum_bounded(pills - usage,
                                    low = 0, high = capacity), by = gr]
    y[, an_end := c(FALSE, inventory[2:N] == 0 & inventory[1:(N-1)] == 0)]
    y[, gr0 := cbin(c(an_end[1:(N-1)] == an_end[2:N], FALSE))]
    z <- y[an_end == TRUE, .(id = id[1],
                             t = t[1],
                             state = null.state), by = gr0][, gr0 := NULL]
    r <- rbind(data[, .(id, t, state)], z)[order(t)]
    if(simplify){
        r[c(TRUE, state[2:.N] != state[1:(.N-1)])]
    } else r
}
