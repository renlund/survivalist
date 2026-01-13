##' treatment burden defined by pill consumption
##'
##' This function is at its core very similar to \code{pill_duration} - please
##' see the documentation for that function also if things are not clear,
##' especially for the argument id, t, pills, usage, and capacity. Here we are
##' only concerned with pills for a single type of drug (unlike
##' \code{pill_duration} there is no \code{state} or \code{null.state}
##' argument). The function will look at consumption of over time and calculate
##' the accumulated burden thereof, either of pills consumed (burden = 'pill')
##' or days with treatment (burden = 'treatment'). It is possible to set a
##' window within which to calculate the burden (e.g. window = 30) as well as a
##' depreciation rate (dep.rate) that determines how fast the accumulated burden
##' (which is a number) should vanish (day by day) when no pills are consumed.
##' @param data a data.frame
##' @param id character; name of id variable
##' @param t character; name of time variable
##' @param pills character; name of variable keeping track of pills added to
##'     inventory
##' @param usage either name of variable (character) keeping track of pill
##'     usage, or the numeric value for all pill usage. If given as a name that
##'     does not exist in data, the value 1 will be assigned.
##' @param capacity either name of variable (character) keeping track of upper
##'     bound for the size of the pill inventory, or the numeric value for all
##'     pill inventory capacities. If given as a name that does not exist in
##'     data, the value Inf will be assigned.
##' @param burden character; 'treatment' for cumulative burden of treatment
##'     (0/1) status, else 'pill' for cumulative burden of pills consumed
##' @param window numeric; how long back to accumulate the burden
##' @param dep.rate numeric; how fast the burden depreciates. If the cumulative
##'     burden has reached V, each unit of time with no treatment will subtract
##'     from the burden by this value, so that the burden is V-dep.rate,
##'     V-2*dep.rate, etc (not being able to go below 0)
##' @param breaks vector of values to cut the burden into (as one would use in
##'     the base function \code{cut}). If no breaks are given, the output will
##'     generally be very long.
##' @param simplify logical; if breaks are given, then simplify = TRUE keep only
##'     minimal information enough to keep track of changes in categorized
##'     burden.
##' @param break.labels character; if breaks are used, this is the 'labels'
##'     argument for \code{base::cut}
##' @seealso \code{\link{pill_duration}}
##' @return If no simplification has been asked for you get a  data.frame
##'  \itemize{
##'  \item{"id"}{ id variable}
##'  \item{"t"}{ time points}
##'  \item{"pills"}{ pills added to inventory}
##'  \item{"capacity"}{ the current capacity for the inventory}
##'  \item{"store.begin"}{ inventory at beginning of day; note that the daily
##' consumption preceeds the need for inventory, so inventory bound (if given)
##' only applies to inventory at the end of day}
##'  \item{"store.end"}{ inventory at end of day}
##'  \item{"use"}{ how many piss were acutally consumed}
##'  \item{"trt"}{ if any pills consumed, one is on (1) treatment}
##'  \item{"stat"}{ the statistic used (trt or use on treatment dats, -dep.rate
##' on non-treatment days)}
##'  \item{"cumstat"}{ the cumulative summation of 'stat' using cumsum_bounded}
##' }
##' @import data.table
##' @export
pill_burden <- function(data, id = "id", t = "t",
                        pills = "pills", usage = "usage",
                        capacity = "capacity",
                        burden = "treatment",
                        window = Inf, dep.rate = 1,
                        breaks = NULL, break.labels = NULL,
                        simplify = TRUE){
    properties(data, class = "data.frame")
    properties(id, class = "character", length = 1, na.ok = FALSE)
    properties(t, class = "character", length = 1, na.ok = FALSE)
    properties(pills, class = "character", length = 1, na.ok = FALSE)
    properties(usage, class = c("character", "numeric", "integer"),
               length = 1, na.ok = FALSE)
    properties(capacity, class = c("character", "numeric", "integer"),
               length = 1, na.ok = FALSE)
    properties(burden, class = "character", length = 1, na.ok = FALSE)
    one_of(burden, set = c("treatment", "pill"))
    properties(window, class = c("numeric", "integer"),
               length = 1, na.ok = FALSE)
    properties(dep.rate, class = c("numeric", "integer"),
               length = 1, na.ok = FALSE)
    if(dep.rate < 0) stop("dep.rate cannot be a negative number")
    if(is.infinite(window) & dep.rate == 0){
        s <- paste0("[pill_burden]: window cannot be Inf ",
                    "at the same as dep.rate = 0")
        stop(s)
    }
    if(!is.null(breaks)){
        properties(breaks, class = c("numeric", "integer"), na.ok = FALSE)
    }
    properties(break.labels, class = c("NULL", "character"), na.ok = FALSE)
    if(!is.null(breaks) & !is.null(break.labels)){
        if(length(break.labels) != length(breaks) - 1){
            s <- paste0("break.labels needs to be of length 1 shorter ",
                        "than the length of breaks as these are passed to ",
                        "base::cut.")
            stop(s)
        }
    }
    properties(simplify, class = "logical", length = 1, na.ok = FALSE)
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
    req_nm <- c(id, t, pills,
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
             new = c("id", "t", "pills", "usage", "capacity"))
    setkey(D, id, t) ## this orders the data accordingly
    R <- rbindlist(l = lapply(X = split(D, f = D$id),
                              FUN = pill_burden_calculator,
                              burden = burden,
                              window = window,
                              dep.rate = dep.rate,
                              breaks = breaks,
                              break.labels = break.labels,
                              simplify = simplify))
    setnames(R, old = c("id", "t"),
             new = c(id, t))
    if(return_dt) R else as.data.frame(R)
}

##' @describeIn pill_burden pill_burden_calculator: The workhorse for
##'     pill_burden; a rigid function that requires a data.table with variables
##'     named 'id' (which should be a constant), 't' (which the data.table
##'     should be ordered on), 'pills', 'usage' and 'capacity'. Probably don't
##'     use this directly, there are no checks or warnings of any kind with this
##'     function
##' @export
pill_burden_calculator <- function(data, burden = "treatment",
                                   window = Inf, dep.rate = 1,
                                   breaks = NULL, break.labels = NULL,
                                   simplify = FALSE){
    if(is.infinite(window) & dep.rate <= 0){
        s <- paste0("[pill_burden_calculator]: window cannot be Inf ",
                    "at the same as dep.rate = 0")
        stop(s)
    }
    if(!is.data.table(data)) data <- as.data.table(data)
    N <- data[, t[.N] - t[1] + 1 +
                sum(ceiling(pills / usage)) +
                if(dep.rate > 0){
                    sum(ceiling(pills / dep.rate))
                } else {
                    window
                }]
    y <- data.table(id = data$id[1], t = data$t[1] + 0:(N-1), pills = 0L)
    y[data, `:=`(## trt = 1L,
                 usage = i.usage,
                 capacity = i.capacity,
                 pills = i.pills), on = "t"]
    y[, `:=`(## trt = locf(trt),
             usage = locf(usage),
             capacity = locf(capacity))]
    y[, store.begin := NA_real_]
    y[, store.end := cumsum_bounded(pills - usage,
                                    low = 0, high = capacity)]
    y[, store.begin := shift(store.end, n = 1, type = "lag", fill = 0) +
            fifelse(pills > capacity,
                    yes = pmin(capacity + usage, pills),
                    no = pills)]
    y[, use := store.begin - store.end]
    y[use == 0, trt := 0L]
    y[, trt := as.integer(use > 0)]

    if(burden == "treatment"){
        y[, stat := fifelse(trt == 1, trt, -dep.rate)]
    } else {
        y[, stat := fifelse(trt == 1, use, -dep.rate)]
    }
    y[, cumstat := cumsum_bounded(stat, low = 0, n = window)]
    if(y[.N, cumstat != 0]){
        s <- paste0("Ooops. If you see this error message it means ",
                    "that the author (who shall remain nameless) ",
                    "was not able to calculate the dimension needed ",
                    "for these calculations. More precisely, the ",
                    "number of rows where not sufficient for the ",
                    "cumulative burden statistic to reach 0.")
        warning(s)
    }
    if(!is.null(breaks)){
        y[, cumstatcat := cut(cumstat, breaks = breaks,
                              labels = break.labels,
                              include.lowest = TRUE)]
        if(simplify){
            y[, tmp := cbin(cumstatcat == shift(cumstatcat, type = "lead"))]
            y[!duplicated(tmp), .(id, t, cumstatcat)]
        } else y[1:last0index(cumstat)]
    } else y[1:last0index(cumstat)]
}

last0index <- function(x){
    z <- x == 0
    rl <- rle(z)
    L <- rl$lengths
    V <- rl$values
    m <- length(L)
    if(isFALSE(V[m])){
        length(x)
    } else {
        if(m == 1) 1 else cumsum(L[1:(m-1)])[m-1] + 1
    }
}

if(FALSE){

    library(data.table)
    library(devtools)
    load_all()

    data <- data.table(
        id = 1,
        t =  c(0,  5, 10),
        pills = c(4, 10,  7),
        usage = c(3,  1,  2),
        capacity = Inf
    )
    burden = "treatment" ## "pill"
    window = Inf
    dep.rate = 1
    breaks = c("zero" = -1, "1-5" = 0, "5-10" = 5, "above 10" = 10, "foo" = Inf)
    simplify = FALSE

    (pbc <- pill_burden_calculator(data, burden, window, dep.rate, breaks, simplify))

    pill_burden(data)
    pill_burden(data, breaks = breaks)

    d2 <- data.table(
        id = 2,
        t =  c(5),
        pills = c(100),
        usage = c(1),
        capacity = 10
    )
    d3 <- data.table(
        id = 3,
        t =  c(5),
        pills = c(10),
        usage = c(100),
        capacity = 4
    )
    d4 <- data.table(
        id = 4,
        t =  c(5),
        pills = c(100),
        usage = c(10),
        capacity = 20
    )
    data2 <- rbind(data, d2, d3, d4)

    pill_burden(data2)
    pill_burden(data2, burden = "pill")
    pill_burden(data2, window = 5)
    pill_burden(data2, dep.rate = 5)
    pill_burden(data2, window = 4, dep.rate = 5)
    pill_burden(data2, breaks = breaks)
    pill_burden(data2, window = 4, dep.rate = 5, breaks = breaks)




}
