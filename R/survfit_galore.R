##' 'survfit' for several outcomes and groups
##'
##' Iterated application of 'survfitted' to all outcomes specificed by 'surv'
##' plus an added possibility of adding (possibly overlapping) groups by
##' supplying a list of indexes in 'glist'.
##' @param formula formula, only RHS of which is used
##' @param data data
##' @param surv specification of surv components
##' @param glist a named list of logical indices
##' @param ... other arguments for survfit
##' @return a data frame
##' @export
survfit_galore <- function(formula, data, surv = NULL, glist = NULL, ...){
    properties(formula, class = "formula")
    properties(data, class = "data.frame")
    properties(surv, class = c("NULL", "character", "list"))
    properties(glist, class = c("NULL", "list"))
    return_dt <- return_data.table(is.data.table(data))
    if(is.null(surv)) surv <- extract_slist_from_names(nm = names(data))
    if(is.character(surv)) surv <- create_slist(s = surv)
    surv <- check_slist(sl = surv, nm = names(data))
    if(is.null(glist)) glist <- list("All" = rep(TRUE, nrow(data)))
    outcome_nm <- rename("outcome", avoid = all.vars(formula))
    group_nm <- rename("group", avoid = all.vars(formula))
    R <- NULL
    fac <- TRUE
    for(G in names(glist)){
        indx <- glist[[G]]
        for(S in names(surv)){
            sf <- survfitted(
                formula = eval(substitute(update(formula, Surv(TIME,EVENT) ~ .),
                                     env = list(TIME = as.name(surv[[S]][1]),
                                                EVENT = as.name(surv[[S]][2])))),
                data = data[indx, ],
                ...
            )
            sf[[outcome_nm]] <- if(fac) factor(S, levels = names(surv)) else S
            sf[[group_nm]] <- if(fac) factor(G, levels = names(glist)) else G
            R <- rbind(R, sf)
        }
    }
    if(return_dt) as.data.table(R) else as.data.frame(R)
}

if(FALSE){

    library(survival)

    d <- gbsg ## data set in survival
    d$group <- factor(d$meno, levels = 0:1, labels = c("Nope", "Yup"))
    gl <- list("Hormon" = gbsg$hormon == 1,
               "No hormon" = gbsg$hormon == 0)

    n <- nrow(d)
    d$ev.foo <- rbinom(n, 1, 0.05)
    d$t.foo <- runif(n, 1, 100)
    d$ev.bar <- rbinom(n, 1, 0.05)
    d$t.bar <- runif(n, 1, 100)
    sl <- list("RF"=c("rfstime","status"),
               "Foo" = c("t.foo", "ev.foo"),
               "Bar" = c("t.bar", "ev.bar"))
    f = ~ grade + group

    str(survfit_galore(formula = f, data = d, surv = sl, glist = gl))

    str(survfit_galore(formula = f, data = d, surv = c("foo", "bar"), glist = gl))

    str(survfit_galore(formula = f, data = d, surv = c("Foo"="foo", "BAR"="bar"), glist = gl))

    str(survfit_galore(formula = f, data = d, surv = NULL, glist = gl))

}
