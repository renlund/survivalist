##' treatment periods defined by fixed durations
##'
##' Given a data frame ('data') with individuals (identified by 'id'), treatment
##' ('state') initiated (oor continued) at 't' with a fixed duration ('run'), we
##' calculate the times at which treatment ends and reverts back to the
##' 'null.state' (or changes). Note 1: a treatement can be cut short by the
##' initiation of a different treatement. Note 2: duplicated times (per
##' individual) will be ignored.
##' @param data a data.frame containing 'id', 't', 'state' and, optionally,
##'     'run'.
##' @param id character; name of id variable
##' @param t character; name of time variable
##' @param state character; name of treatment variable
##' @param run either name of variable (character) containing treatment
##'     durations, or the numeric value for all treatment durations
##' @param null.state value for 'state' when treatment ends
##' @param simplify logical; if TRUE keep only minimal information enough to
##'     keep track of changes in treatment.
##' @return a data.frame
##' @examples
##' df <- data.frame(id = 1,
##'                  t = c(0, 10, 100, 110),
##'                  state = LETTERS[c(1, 1, 1, 2)])
##' fixed_treatment(data = df, run = 50, simplify = TRUE)
##' fixed_treatment(data = df, run = 50, simplify = FALSE)
##' @import data.table
##' @export
fixed_treatment <- function(data, id = "id", t = "t", state = "state",
                            run = "run", null.state = "", simplify = TRUE){
    ## check arguments:
    properties(data, class = "data.frame")
    properties(id, class = "character", length = 1, na.ok = FALSE)
    properties(t, class = "character", length = 1, na.ok = FALSE)
    properties(state, class = "character", length = 1, na.ok = FALSE)
    properties(run, class = c("character", "numeric", "integer"),
               length = 1, na.ok = FALSE)
    properties(null.state, length = 1, na.ok = FALSE)
    properties(simplify, class = "logical", length = 1, na.ok = FALSE)

    ## return_dt <- is.data.table(data)
    return_dt <- return_data.table(is.data.table(data))
    rchar <- is.character(run)
    req_nm <- c(id, t, state, if(rchar) run else NULL)
    inclusion(names(data), nm = "data", include = req_nm)
    D <- as.data.table(data)[, req_nm, with = FALSE]
    if(!rchar){
        req_nm <- c(req_nm, "run")
        D[, run := run]
    }
    if(D[, any(run <= 0)]){
        s <- paste0("run must be a (strictly) positive variable")
    }
    setnames(D, old = req_nm, new = c("id", "t", "state", "run"))
    setkey(D, id, t)
    R <- rbindlist(l = lapply(X = split(D, f = D$id),
                              FUN = fixed_switch,
                              null.state = null.state,
                              simplify = simplify),
                   use.names = TRUE, fill = TRUE)
    setnames(R, old = c("id", "t", "state"), new = c(id, t, state))
    if(return_dt) R else as.data.frame(R)
}

##' @describeIn fixed_treatment The workhorse for fixed_treatment; a rigid
##'     function that requires a data.table with variables named 'id' (which
##'     should be a constant), 't' (which the data.table should be ordered on),
##'     'state', and 'run'. Probably don't use this directly, there are no
##'     checks or warnings of any kind with this function
##' @export
fixed_switch <- function(data, null.state = "", simplify = TRUE){
    data <- data[!duplicated(t), .(id, t, state, run)]
    n <- nrow(data)
    if(n == 1){
        data.table(id = data$id, t = c(data$t, data$t + data$run),
                   state = c(data$state, null.state))
    } else {
        y <- data[run < c(t[2:n] - t[1:(n-1)], Inf)][
          , `:=`(t = t + run, state = null.state, run = NULL)]
        data[, `:=`(run = NULL)]
        z <- rbind(data, y)[order(t)]
        if(simplify){
            m <- nrow(z)
            z[c(TRUE, state[2:m] != state[1:(m-1)])]
        } else z
    }
}


##' @describeIn fixed_treatment A wrapper for fixed_treatment where the only
##'     state is 1 (and not explicity in the data) and the null state is 0.
##' @export
onoff_treatment <- function(data, id = "id", t = "t", run = "run",
                            simplify = TRUE){
    fixed_treatment(as.data.frame(data)[, state := 1L],
                    id = d,
                    t = t,
                    state = "state",
                    run = run,
                    null.state = 0L,
                    simplify = simplify)
}
