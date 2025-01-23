##' 'survfit' for several outcomes and groups
##'
##' Iterated application of 'survfitted' to all outcomes specificed by 'surv'
##' plus an added possibility of adding (possibly overlapping) groups by
##' supplying a list of indexes in a grouping table (gtab).
##' @param formula formula, of which only the RHS is used
##' @param data data.frame; the data
##' @param surv specification of surv components (NULL, character vector, or
##'     stab)
##' @param gtab data.frame; a grouping table (gtab)
##' @param ... other arguments for survfit. Note: you can pass 'tp' for
##'     calculation of at risk numbers, survfit_galore will capture these
##' @return a data frame
##' @export
survfit_galore <- function(formula, data, surv = NULL, gtab = NULL, ...){
    properties(formula, class = "formula")
    properties(data, class = "data.frame")
    properties(surv, class = c("NULL", "character", "data.frame"))
    properties(gtab, class = c("NULL", "data.frame"))
    return_dt <- return_data.table(is.data.table(data))
    if(is.null(surv)) surv <- extract_stab_from_names(nm = names(data))
    if(is.character(surv)) surv <- create_stab(s = surv)
    surv <- verify_stab(stab = surv, nm = names(data))
    if(is.null(gtab)) gtab <- data.table("All" = rep(TRUE, nrow(data)))
    outcome_nm <- rename("outcome", avoid = all.vars(formula))
    group_nm <- rename("group", avoid = all.vars(formula))
    R <- NULL
    fac <- TRUE ## this could be an option
    dots <- list(...) ## dots <- as.list(NULL) ## dots <- list(tp=tp)
    for(G in names(gtab)){ ## G = names(gtab[1])
        indx <- gtab[[G]]
        for(i in seq_along(surv$label)){ ## i = 1
            S <- surv$label[i]
            dots$formula <- eval(substitute(
                update(formula, Surv(TIME,EVENT) ~ .),
                env = list(TIME = as.name(surv$time[i]),
                           EVENT = as.name(surv$event[i]))))
            dots$data <- data[indx, ]
            sf <- do.call(what = survfitted, args = dots)
            ## sf <- survfitted(
            ##     formula = eval(substitute(update(formula, Surv(TIME,EVENT) ~ .),
            ##                          env = list(TIME = as.name(surv$time[i]),
            ##                                     EVENT = as.name(surv$event[i])))),
            ##     data = data[indx, ],
            ##     ...
            ## )
            sf[[outcome_nm]] <- if(fac) factor(S, levels = surv$label) else S
            sf[[group_nm]] <- if(fac) factor(G, levels = names(gtab)) else G
            ar <- attr(sf, "at_risk")
            if( !is.null(ar) ){
                ar[[outcome_nm]] <- if(fac) factor(S, levels = surv$label) else S
                ar[[group_nm]] <- if(fac) factor(G, levels = names(gtab)) else G
                ## ar[, `:=`(outcome = if(fac) factor(S, levels = surv$label) else S,
                ##           group = if(fac) factor(G, levels = names(gtab)) else G)]
                AR <- rbind(attr(R, "at_risk"), ar)
                R <- rbind(R, sf)
                attr(R, "at_risk") <- AR
            } else R <- rbind(R, sf)
        }
    }
    if(return_dt) as.data.table(R) else as.data.frame(R)
}

if(FALSE){

    rm(list=ls())
    library(survival)

    d <- gbsg ## data set in survival
    d$group <- factor(d$meno, levels = 0:1, labels = c("Nope", "Yup"))
    gl <- data.frame("Hormon" = gbsg$hormon == 1,
                     "No hormon" = gbsg$hormon == 0)

    n <- nrow(d)
    d$ev.foo <- rbinom(n, 1, 0.05)
    d$t.foo <- runif(n, 1, 100)
    d$ev.bar <- rbinom(n, 1, 0.05)
    d$t.bar <- runif(n, 1, 100)
    sl <- data.frame(label = c("RF", "Foo", "Bar"),
                     time = c("rfstime", "t.foo", "t.bar"),
                     event = c("status", "ev.foo", "ev.bar"))
    f = ~ grade + group

    str(survfit_galore(formula = f, data = d, surv = sl, gtab = gl))

    test <- survfit_galore(formula = f, data = d, surv = sl, gtab = gl,
                           tp = c(0,1000,5000))
    str(attr(test, "at_risk"))

    str(survfit_galore(formula = f, data = d, surv = c("foo", "bar"), gtab = gl))

    str(survfit_galore(formula = f, data = d, surv = c("Foo"="foo", "BAR"="bar"), gtab = gl))

    str(survfit_galore(formula = f, data = d, surv = NULL, gtab = gl))

}
