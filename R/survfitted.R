##' wrapper for 'survfit'
##'
##' This wrapper for survival::survfit.formula splits the resulting 'strata'
##' component into its constituting variables, allows for rescaling and can
##' calculate numbers at risk.
##' @param formula formula passed (to survival::survfit.formula)
##' @param data data passed
##' @param ... other arguments passed
##' @param time.unit numerical; scale for time, e.g. set to 365.25 to convert
##'     from days o years
##' @param keep.factors logical; keep factors in formula as factors in the
##'     output
##' @param tp numerical vector; the time points (tp) for which to calculate at risk
##'     numbers. These will be attached as attribute 'at_risk' to the output.
##' @return a data frame
##' @export
survfitted <- function(formula, data, ..., time.unit = 1L,
                       keep.factors = TRUE, tp = NULL){
    properties(formula, nm = "The formula", class = "formula")
    properties(data, class = "data.frame")
    properties(keep.factors, class = "logical", length = 1, na.ok = FALSE)
    properties(time.unit, class = c("numeric", "integer"),
               length = 1, na.ok = FALSE)
    properties(tp, class = c("NULL", "numeric", "integer"), na.ok = FALSE)
    return_dt <- return_data.table(is.data.table(data))
    fac <- as.list(NULL)
    av <- all.vars(formula)
    avoidance(av, nm = "formula variables",
              avoid = names(survfit2df(NULL)))
    if(keep.factors){
        for(v in av){
            if(is.factor(data[[v]])) fac[[v]] <- levels(data[[v]])
        }
    }
    dots <- list(...) ## dots <- as.list(NULL)
    dots$formula <- formula
    dots$data <- data
    tmp_sf <- do.call(what = survival::survfit.formula, args = dots)
    sf <- survival::survfit0(tmp_sf)
    x <- sf$strata
    if(is.null(x)){
        survfit2df(sf)
    } else {
        y <- names(x)
        ys <- strsplit(y, split = "(=|, )")
        N <- length(ys)
        test <- ys[[1]]
        n <- length(test)
        v <- test[seq(1, n-1, by = 2)]
        indx <- seq(2,n, by = 2)
        M <- V <- as.list(NULL)
        for(j in indx){
            z <- convert2NumbersIfSuitable(unlist(lapply(ys, FUN = function(x) x[j])))
            V[[v[j/2]]] <- unique(z)
            M[[v[j/2]]] <- rep(z, x)
        }
        R <- cbind(survfit2df(sf), as.data.table(M))
        if(length(fac) != 0){
            for(v in names(fac)){
                R[[v]] <- factor(R[[v]], levels = fac[[v]])
            }
        }
        R[, time := time / time.unit]
        if(!is.null(tp)){
            at_risk <- tryCatch(
                expr = {
                    eg <- do.call(what = expand.grid,
                                  args = c(V, list(stringsAsFactors = FALSE)))
                    n <- nrow(eg)
                    m <- ncol(eg)
                    N <- nrow(R)
                    AR <- NULL
                    for(i in 1:n){ ## i = 1
                        filt <- rep(TRUE, N)
                        for(j in 1:m){ ## j = 2
                            filt <- filt & R[, foo == eg[i,j],
                                             env = list(foo = names(eg)[j])]
                        }
                        for(j in seq_along(tp)){ ## j = 1
                            t <- tp[j]
                            r <- R[filt &
                                   time >= t][
                                which.min(time)][
                              , .(tp = t, time, n.risk)]
                            if(nrow(r) == 0){
                                break
                            } else {
                                AR <- rbind(AR, cbind(eg[i,, drop=FALSE], r))
                            }
                        }
                    }
                    AR
                },
                error = function(e){
                    w <- paste0("at risk calculation failed")
                    message(w)
                    NULL
                }
            )
            attr(R, "at_risk") <- if(return_dt){
                                      at_risk
                                  } else as.data.frame(at_risk)
        }
        if(return_dt) R else as.data.frame(R)
    }
}

survfit2df <- function(s = NULL, dt = TRUE){
    R <- if(!is.null(s)){
             properties(s, class = "survfit")
             inclusion(names(s), nm = "the survfit object",
                       include = c("time", "surv", "lower", "upper",
                                   "n.risk", "n.event", "n.censor", "std.err"))
             data.frame(
                 'time' = s$time,
                 'estimate' = s$surv, ## should we remove this?
                 'surv' = s$surv,
                 'ci.low' = s$lower,
                 'ci.high' = s$upper,
                 'n.risk' = s$n.risk,
                 'n.event' = s$n.event,
                 'n.censor' = s$n.censor,
                 'std.error' = s$std.err,
                 'cumhaz' = s$cumhaz,
                 'std.chaz' = s$std.chaz,
                 stringsAsFactors = FALSE
             )
         } else {
             data.frame(
                 'time' = NA_real_,
                 'estimate' = NA_real_, ## should we remove this?
                 'surv' = NA_real_,
                 'ci.low' = NA_real_,
                 'ci.high' = NA_real_,
                 'n.risk' = NA_real_,
                 'n.event' = NA_real_,
                 'n.censor' = NA_real_,
                 'std.error' = NA_real_,
                 'cumhaz' = NA_real_,
                 'std.chaz' = NA_real_,
                 stringsAsFactors = FALSE
             )
         }
    if(dt) as.data.table(R) else R
}

convert2NumbersIfSuitable <- function(x){
    a <- tryCatch(expr = {
        n <- as.numeric(x)
        i <- as.integer(x)
        y <- if(all(n == i)) i else n
        TRUE
    }, warning = function(w) FALSE)
    if(a) y else x
}



if(FALSE){

    library(survival)

    d <- survival::gbsg ## data set in survival
    d$group <- factor(d$meno, levels = 0:1, labels = c("Nope", "Yup"))
    gl <- list("Hormon" = d$hormon == 1,
               "No hormon" = d$hormon == 0)
    f <- Surv(rfstime, status) ~ grade + group
    d$vikt <- runif(nrow(d), 0.1, 1)

    sf <- survfit(formula = f, data = d)
    survfit2df(sf)

    ## survfitted(formula = y ~ time + estimate, data = d)

    formula = f
    keep.factors = TRUE
    data = d
    ls()
    rm(f,v,d,sf)

    str(survfitted(formula = f, data = d))
    str(survfitted(formula = f, data = d, keep.factors = FALSE))
    str(test <- survfitted(formula = f, data = d, tp = c(0,400, 800)))
    attr(test, "at_risk")
    test <- survfitted(formula = f, data = d, time.unit = 365.25,
                           tp = c(0,2.5, 5, 7.5, 10))
    attr(test, "at_risk")

    test <- survfitted(formula = Surv(rfstime, status) ~ grade,
                       data = d, time.unit = 365.25,
                       tp = c(0,2.5, 5, 7.5, 10))
    attr(test, "at_risk")




    str(survfitted(formula = f, data = d, weights = v))
    str(survfitted(formula = f, data = d, weights = d$vikt))

}
