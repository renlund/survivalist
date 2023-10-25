##' transform surv variables
##'
##' Different functions for transforming surv variables: combine, truncate,
##' rescale
##' @param surv specification of surv components
##' @param data the data
##' @param id name of id variable (optional)
##' @param nm name of time and status component of variable created
##'     (combine_surv)
##' @param FUN function for rescaling, e.g. function(x) x/365.25 if your time
##'     components are originally measured in days and you want them to be
##'     measured in years (rescale_surv)
##' @param strip logical, strip everything from data apart from the combined
##'     surv calculated and id (if specified)?
##' @param trunc numeric, the time to truncate at (truncate_surv)
##' @name transform_surv
NULL

##' @rdname transform_surv
##' @details combine_surv: The combined surv variable will take the time from
##'     the smallest time-component of the composing variables. It will have
##'     status 1 if any of the event components whos corresponding time equals
##'     the smallest time is 1, else 0.
##' @export
combine_surv <- function(surv, data, id = NULL, nm = surv_nm("Combined"),
                         strip = TRUE){
    properties(surv, class = c("NULL", "character", "list"))
    properties(data, class = "data.frame")
    properties(id, class = c("NULL", "character"), length = 0:1, na.ok = FALSE)
    if(is.character(id)) inclusion(names(data), nm = "data names", include = id)
    properties(nm, class = "character", length = 2, na.ok = FALSE)
    if(is.null(surv)) surv <- extract_slist_from_names(names(data))
    if(is.character(surv)) surv <- create_slist(s = surv)
    sl <- check_slist(sl = surv, nm = names(data))
    return_dt <- return_data.table(is.data.table(data))
    ## if(!is.data.table(data)) data <- as.data.table(data)
    data <- as.data.table(data)
    ts <- slist_time(sl)
    es <- slist_event(sl)
    times <- data[, ts, with = FALSE]
    events <- data[, es, with = FALSE]
    min_t <- do.call(pmin, times)
    max_ev <- as.integer(rowSums((times == min_t) * (events)) > 0)
    data[, (nm) := .(min_t, max_ev)]
    ## if(!is.null(id)){
    ##     r <- data.table(data[[id]], min_t, max_ev)
    ##     setnames(r, new = c(id, nm))
    ## } else {
    ##     r <- data.table(min_t, max_ev)
    ##     setnames(r, new = nm)
    ## }
    r <- if(strip){
        vs <- c(id, nm)
        data[, vs, with = FALSE]
    } else data
    if(return_dt) r else as.data.frame(r)
}

##' @rdname transform_surv
##' @details rescale_surv: apply function to time component of surv variables
##' @export
rescale_surv <- function(surv = NULL, data, FUN, id = NULL, strip = TRUE){
    properties(surv, class = c("NULL", "character", "list"))
    properties(data, class = "data.frame")
    properties(FUN, class = "function", length = 1)
    properties(id, class = c("NULL", "character"), length = 0:1, na.ok = FALSE)
    properties(strip, class = "logical", length = 1, na.ok = FALSE)
    if(is.character(id)) inclusion(names(data), nm = "data names", include = id)
    if(is.null(surv)) surv <- extract_slist_from_names(names(data))
    if(is.character(surv)) surv <- create_slist(s = surv)
    sl <- check_slist(sl = surv, nm = names(data))
    return_dt <- return_data.table(is.data.table(data))
    ## if(!is.data.table(data)) data <- as.data.table(data)
    data <- as.data.table(data)
    ts <- slist_time(sl)
    data[, (ts) := lapply(.SD, FUN), .SDcols = ts]
    r <- if(strip){
        es <- slist_event(sl)
        vs <- c(id, shuffle(ts, es))
        data[, vs, with = FALSE]
    } else data
    if(return_dt) r else as.data.frame(r)
}

shuffle <- function(x, y){
    n <- length(x)
    if(length(y) != n) stop("no shuffle for you!")
    N <- 2*n
    r <- rep(NA, N)
    r[seq(1, N, 2)] <- x
    r[seq(2, N, 2)] <- y
    r
}

##' @rdname transform_surv
##' @details trunacate_surv: truncate surv variables
##' @export
truncate_surv <- function(surv = NULL, data, trunc, id = NULL, strip = TRUE){
    properties(surv, class = c("NULL", "character", "list"))
    properties(data, class = "data.frame")
    properties(trunc, class = c("numeric", "integer"), length = 1, na.ok = FALSE)
    properties(id, class = c("NULL", "character"), length = 0:1, na.ok = FALSE)
    properties(strip, class = "logical", length = 1, na.ok = FALSE)
    if(is.character(id)) inclusion(names(data), nm = "data names", include = id)
    if(is.null(surv)) surv <- extract_slist_from_names(names(data))
    if(is.character(surv)) surv <- create_slist(s = surv)
    sl <- check_slist(sl = surv, nm = names(data))
    return_dt <- return_data.table(is.data.table(data))
    ## if(!is.data.table(data)) data <- as.data.table(data)
    data <- as.data.table(data)
    ts <- slist_time(sl)
    es <- slist_event(sl)
    for(i in seq_along(ts)){
        eval(expr = substitute(
            expr = data[, c(foo_ch, bar_ch) := {
                .(pmin(foo, trunc), fifelse(bar == 1 & foo <= trunc, 1L, 0L))
            }],
            env = list(foo_ch = ts[i],
                       foo = as.name(ts[i]),
                       bar_ch = es[i],
                       bar = as.name(es[i]))
        ))
    }
    r <- if(strip){
        vs <- c(id, shuffle(ts, es))
        data[, vs, with = FALSE]
    } else data
    if(return_dt) r else as.data.frame(r)
}

##' @rdname transform_surv
##' @details Survclass_surv: create variables of class 'Surv'
##' @export
Survclass_surv <- function(surv = NULL, data, id = NULL, strip = TRUE){
    properties(surv, class = c("NULL", "character", "list"))
    properties(data, class = "data.frame")
    properties(id, class = c("NULL", "character"), length = 0:1, na.ok = FALSE)
    properties(strip, class = "logical", length = 1, na.ok = FALSE)
    if(is.character(id)) inclusion(names(data), nm = "data names", include = id)
    if(is.null(surv)) surv <- extract_slist_from_names(names(data))
    if(is.character(surv)) surv <- create_slist(s = surv)
    sl <- check_slist(sl = surv, nm = names(data))
    rm <- c(slist_time(sl), slist_event(sl))
    DATA <- subset(as.data.frame(data), subset = TRUE,
                   select = setdiff(names(data), rm))
    for(i in seq_along(sl)){
        DATA[[names(sl)[i]]] <- survival::Surv(time = data[[sl[[i]][1]]],
                                               event = data[[sl[[i]][2]]])
    }
    if(strip){
        vs <- c(id, names(sl))
        DATA[, vs]
    } else {
        DATA
    }
}

if(FALSE){

    data <- data.frame(
        the_id = 1:3,
        X_t = c(10, 7, 5),
        X_e = c( 1, 0, 1),
        Y_t = c(10, 7, 9),
        Y_e = c( 0, 1, 0),
        noise = letters[1:3]
    )
    surv <- list(c("X_t", "X_e"), c("Y_t", "Y_e"))
    trunc = 8
    id = "the_id"
    strip = FALSE

    Survclass_surv(surv, data)
    Survclass_surv(surv, data, strip = FALSE)

    data <- data.frame(
        id = 1:7,
        foo_t =  c(10,10,10,7,9,5,5),
        foo_ev = c(0,0,0,1,1,1,0),
        bar_t =  c(9,10,8,10,6,5,6),
        bar_ev = c(0,0,1,0,1,1,1)
    )

    sl <- list(Foo = c("foo_t", "foo_ev"),
               Bar = c("bar_t", "bar_ev"))
    combined_surv(surv = sl,
                  data = data,
                  id = "id",
                  nm = c("Comb_t", "Comb_ev"))
    as.data.table(data)[, (surv_nm("Comb")) :=(
                     combined_surv(surv = sl,
                                   data = .SD)
                 )]

}
