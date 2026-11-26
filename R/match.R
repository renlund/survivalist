##' create matches by x
##'
##' Match controls to cases by how similar they are on the covariate x. This is
##' a greedy matching procedure that might fail. Best case scenario is that it
##' is fast.
##' @param x character; name of numeric variable in data
##' @param case character; name of variable defining cases and controls
##' @param id character; name of id variable
##' @param data data.frame or similar
##' @param ctrl.bound integer value; the number of controls you are hoping for
##'     per case
##' @param case.val the value of the 'case' variable that defines the cases
##'     (others are controls)
##' @return a data frame with id, case, x and a variable 'cluster' that defines
##'     the comparison clusters identified
##' @export
match_by_x <- function(x, case, id, data, ctrl.bound = 1, case.val = 1){
    properties(x, class = "character", length = 1, na.ok = FALSE)
    properties(id, class = "character", length = 1, na.ok = FALSE)
    properties(data, class = "data.frame")
    terms <- c(id, case, x)
    inclusion(names(data), nm = "names of data", include = terms)
    if(!is.numeric(data[[x]])){
        stop("x needs to be numeric")
    }
    properties(case.val, length = 1, na.ok = FALSE)
    ctrl.bound <- type.convert(ctrl.bound, as.is = FALSE)
    properties(ctrl.bound, class = "integer", length = 1, na.ok = FALSE)
    if(ctrl.bound < 1) stop("use ctrl.bound > 0")
    d <- as.data.table(data)[, terms, with = FALSE]
    new_names <- c("id", "case", "x")
    setnames(d, old = terms, new = new_names)
    CASE <- d[case == case.val]
    CTRL <- d[case != case.val]
    dummy.bound <- nrow(CASE) * ctrl.bound + 1
    R <- NULL
    dummy <- 0L
    while( nrow(CASE) > 0 & nrow(CTRL) > 0 & dummy < dummy.bound){
        dummy <- dummy + 1L
        foo <- function(ctrl, case){
            ctrl[case, .(case = i.id, x.case = i.x,
                         ctrl = x.id, x.ctrl = x.x),
                 on = "x", roll = "nearest", allow.cartesian = TRUE][!duplicated(ctrl)]
        }
        R <- rbind(R, foo(CTRL, CASE))
        KILL <- R[, .(count = .N), by = case][count >= ctrl.bound]
        if(nrow(KILL) > 0) CASE <- CASE[!KILL, on = .(id = case)]
        CTRL <- CTRL[!R, on = .(id = ctrl)]
    }
    if(any(duplicated(R$ctrl))){
        s <- paste0("This shouldn't happen. The coder (who shall remain ",
                    "nameless) has not been able to choose controls ",
                    "without replacement. Why? WHYY?")
        warning(s)
    }
    setorder(R, case, x.ctrl)
    EVAL <- R[, .(n = .N), by = case][, .(n2 = .N), by = n]
    if(EVAL[, any(n != ctrl.bound)]){
        s <- paste0("\nDid not find exactly ", ctrl.bound, " controls ",
                    "for each case. Check the results.\n")
        message(s)
    }
    d[, cluster := fifelse(case == case.val, yes = id, no = NA)]
    d[R, cluster := i.case, on = .(id = ctrl)]
    setnames(d, old = new_names, new = terms)
    d
}

if(FALSE){

    rm(list=ls())
    library(data.table)
    N <- 100000
    n <- 10000
    set.seed(1)
    X <- data.table(lopnr = 1:N, ps = runif(N, 0.01, 0.99), trt = 0L)
    X[sample(1:.N, size = n), trt := 1L]

    r <- match_by_x(x = "ps", case = "trt", id = "lopnr", data =X,
                    ctrl.bound = 2)
    setorder(r, cluster)
    r[!is.na(cluster)]

    ## x = "ps"
    ## case = "trt"
    ## id = "lopnr"
    ## data = copy(X)
    ## ctrl.bound = 3
    ## case.val = 1


}
