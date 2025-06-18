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
    properties(surv, class = c("NULL", "character", "data.frame"))
    properties(data, class = "data.frame")
    properties(id, class = c("NULL", "character"), length = 0:1, na.ok = FALSE)
    if(is.character(id)) inclusion(names(data), nm = "data names", include = id)
    properties(nm, class = "character", length = 2, na.ok = FALSE)
    if(is.null(surv)) surv <- extract_stab_from_names(names(data))
    if(is.character(surv)) surv <- create_stab(s = surv)
    stab <- verify_stab(stab = surv, nm = names(data))
    return_dt <- return_data.table(is.data.table(data))
    data <- as.data.table(data)
    times <- data[, stab$time, with = FALSE]
    events <- data[, stab$event, with = FALSE]
    min_t <- do.call(pmin, times)
    max_ev <- as.integer(rowSums((times == min_t) * (events)) > 0)
    data[, (nm) := .(min_t, max_ev)]
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
    properties(surv, class = c("NULL", "character", "data.frame"))
    properties(data, class = "data.frame")
    properties(FUN, class = "function", length = 1)
    properties(id, class = c("NULL", "character"), length = 0:1, na.ok = FALSE)
    properties(strip, class = "logical", length = 1, na.ok = FALSE)
    if(is.character(id)) inclusion(names(data), nm = "data names", include = id)
    if(is.null(surv)) surv <- extract_stab_from_names(names(data))
    if(is.character(surv)) surv <- create_stab(s = surv)
    stab <- verify_stab(stab = surv, nm = names(data))
    return_dt <- return_data.table(is.data.table(data))
    data <- as.data.table(data)
    data[, (stab$time) := lapply(.SD, FUN), .SDcols = stab$time]
    r <- if(strip){
        vs <- c(id, shuffle(stab$time, stab$event))
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
##' @details truncate_surv: truncate surv variables
##' @export
truncate_surv <- function(surv = NULL, data, trunc, id = NULL, strip = TRUE){
    properties(surv, class = c("NULL", "character", "data.frame"))
    properties(data, class = "data.frame")
    properties(trunc, class = c("numeric", "integer"), length = 1, na.ok = FALSE)
    properties(id, class = c("NULL", "character"), length = 0:1, na.ok = FALSE)
    properties(strip, class = "logical", length = 1, na.ok = FALSE)
    if(is.character(id)) inclusion(names(data), nm = "data names", include = id)
    if(is.null(surv)) surv <- extract_stab_from_names(names(data))
    if(is.character(surv)) surv <- create_stab(s = surv)
    stab <- verify_stab(stab = surv, nm = names(data))
    return_dt <- return_data.table(is.data.table(data))
    data <- as.data.table(data)
    for(i in seq_along(stab$time)){
        eval(expr = substitute(
            expr = data[, c(foo_ch, bar_ch) := {
                .(pmin(foo, trunc), fifelse(bar == 1 & foo <= trunc, 1L, 0L))
            }],
            env = list(foo_ch = stab$time[i],
                       foo = as.name(stab$time[i]),
                       bar_ch = stab$event[i],
                       bar = as.name(stab$event[i]))
        ))
    }
    r <- if(strip){
        vs <- c(id, shuffle(stab$time, stab$event))
        data[, vs, with = FALSE]
    } else data
    if(return_dt) r else as.data.frame(r)
}

##' @rdname transform_surv
##' @details Survclass_surv: create variables of class 'Surv'
##' @export
Survclass_surv <- function(surv = NULL, data, id = NULL, strip = TRUE){
    properties(surv, class = c("NULL", "character", "data.frame"))
    properties(data, class = "data.frame")
    properties(id, class = c("NULL", "character"), length = 0:1, na.ok = FALSE)
    properties(strip, class = "logical", length = 1, na.ok = FALSE)
    if(is.character(id)) inclusion(names(data), nm = "data names", include = id)
    if(is.null(surv)) surv <- extract_stab_from_names(names(data))
    if(is.character(surv)) surv <- create_stab(s = surv)
    stab <- verify_stab(stab = surv, nm = names(data))
    rm <- c(stab$time, stab$event)
    DATA <- subset(as.data.frame(data), subset = TRUE,
                   select = setdiff(names(data), rm))

    for(i in seq_along(stab$label)){
        DATA[[stab$label[i]]] <- survival::Surv(time = data[[stab$time[i]]],
                                               event = data[[stab$event[i]]])
    }
    if(strip){
        vs <- c(id, stab$label)
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
    surv <- data.frame(label = c("theX", "theY"),
                       time = c("X_t", "Y_t"),
                       event = c("X_e", "Y_e"))
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

    surv <- data.frame(label = c("Foo", "Bar"),
                       time = c("foo_t", "bar_t"),
                       event = c("foo_ev", "bar_ev"))
    combine_surv(surv = surv,
                 data = data,
                 id = "id",
                 nm = c("Comb_t", "Comb_ev"))
    as.data.table(data)[, (surv_nm("Comb")) :=(
                     combine_surv(surv = surv,
                                   data = .SD)
    )]

    rescale_surv(surv = surv,
                 data = data,
                 FUN = function(x) 10*x,
                 strip = FALSE)

    truncate_surv(surv = surv,
                  data = data,
                  trunc = 8,
                  strip = FALSE)


}
