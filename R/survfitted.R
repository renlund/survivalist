##' wrapper for 'survfit'
##'
##' This wrapper for survival::survfit.formula only splits the resulting 'strata'
##' component into its constituting variables
##' @param formula formula passed (to survival::survfit.formula)
##' @param data data passed
##' @param ... other arguments passed
##' @param keep.factors logical; keep factors in formula as factors in the output
##' @return a data frame
##' @export
survfitted <- function(formula, data, ..., keep.factors = TRUE){
    properties(formula, nm = "The formula", class = "formula")
    properties(data, class = "data.frame")
    properties(keep.factors, class = "logical", length = 1, na.ok = FALSE)
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
        M <- as.list(NULL)
        for(j in indx){
            M[[v[j/2]]] <- rep(convert2NumbersIfSuitable(
                unlist(lapply(ys, FUN = function(x) x[j]))
            ), x)
        }
        R <- cbind(survfit2df(sf), as.data.table(M))
        if(length(fac) != 0){
            for(v in names(fac)){
                R[[v]] <- factor(R[[v]], levels = fac[[v]])
            }
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
                 'estimate' = s$surv,
                 'ci.low' = s$lower,
                 'ci.high' = s$upper,
                 'n.risk' = s$n.risk,
                 'n.event' = s$n.event,
                 'n.censor' = s$n.censor,
                 'std.error' = s$std.err,
                 stringsAsFactors = FALSE
             )
         } else {
             data.frame(
                 'time' = NA_real_,
                 'estimate' = NA_real_,
                 'ci.low' = NA_real_,
                 'ci.high' = NA_real_,
                 'n.risk' = NA_real_,
                 'n.event' = NA_real_,
                 'n.censor' = NA_real_,
                 'std.error' = NA_real_,
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

    d <- gbsg ## data set in survival
    d$group <- factor(d$meno, levels = 0:1, labels = c("Nope", "Yup"))
    gl <- list("Hormon" = gbsg$hormon == 1,
               "No hormon" = gbsg$hormon == 0)
    f <- Surv(rfstime, status) ~ grade + group
    v <- runif(nrow(d), 0.1, 1)
    d$vikt <- v

    survfitted(formula = y ~ time + estimate, data = d)
    str(survfitted(formula = f, data = d))
    str(survfitted(formula = f, data = d, keep.factors = FALSE))
    str(survfitted(formula = f, data = d, weights = v))
    str(survfitted(formula = f, data = d, weights = d$vikt))

}
