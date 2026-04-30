# NOTE: these functions imported from earlier package sensias. Could use an oversight.

##' Bias analysis for cox regression
##'
##' Get estimates of 'true' effect given the addition of a confounder, as
##' calculated by \code{update_HR}.
##' @param data data frame
##' @param surv an 'stab' with 1 row or, if surv pair can be identified through
##'     the default affix, the common name (label) for the pair
##' @param main name of main effect (binary)
##' @param bnry names of binary variables to include
##' @param real names of continuous 'normalish' variables to include
##' @param xtra.adj extra string to include in formula
##' @param bnry.manual additional binary confounders not in model, named list
##'     where each element is HR, proportion at \code{main == 0}, proportion at
##'     \code{main == 1}
##' @param real.manual additional real confounders not in model, named list
##'     where each element is HR, mean at \code{main == 0}, mean at \code{main
##'     == 1}
##' @return a data.frame with entries \itemize{
##'
##'  \item{"term"}{The term (variable)}
##'
##'  \item{"type"}{Type of variable: main (the main effect), bnry (binary), real
##'                (continuous). Terms added via 'xtra.adj' will have NA here.}
##'
##'  \item{"manual"}{Indicator for 'manually added' variables, i.e. hypothesized
##'         variables.}
##'
##'  \item{"stat0"}{Mean value for variable at main = 0}
##'
##'  \item{"stat1"}{Mean value for variable at main = 1}
##'
##'  \item{"adjHR (adjHR.l, adjHR.u)"}{The HR (lower, upper CI) in an adjusted
##'         model (terms specified by bnry, real, and xtra.adj). Note: values
##'         are included here for the 'manual' terms although they do not
##'         actually appear in the model. }
##'
##'  \item{"mainHR (mainHR.l, mainHR.u)"}{The HR for main if a confounder with
##'         the parameters specified (stat0, stat1, adjHR) was present.}
##'
##'  \item{"mainHRinv (mainHRinv.l, mainHRinv.u)"}{Similar to 'mainHR' but the
##'         effect of the confounder is inversed}
##'
##' }
##' Also, there is an attribute 'tidy' with easy-to-plot information.
##' @export
coxreg_bias <- function(data, surv = NULL, main,
                        bnry = NULL, real = NULL, xtra.adj = NULL,
                        bnry.manual = NULL, real.manual = NULL){
    properties(data, class = "data.frame")
    properties(surv, class = c("NULL", "character", "data.frame"))
    properties(main, class = "character", length = 1, na.ok = FALSE)
    if(!is.null(bnry)) properties(bnry, class = "character", na.ok = FALSE)
    if(!is.null(real)) properties(real, class = "character", na.ok = FALSE)
    if(!is.null(xtra.adj)) properties(xtra.adj, class = "character", na.ok = FALSE)
    if(!is.null(bnry.manual)) properties(bnry.manual, class = "list")
    if(!is.null(real.manual)) properties(real.manual, class = "list")
    if(is.null(surv)){
        surv <- extract_stab_from_names(nm = names(data))
        if(is.null(surv)){
            stop("can not identify any surv pairs in data")
        }
    }
    if(is.character(surv)) surv <- create_stab(s = surv)
    surv <- verify_stab(stab = surv, nm = names(data))
    if(nrow(surv) > 1){
        s <- paste0("several surv pairs can be identified, ",
                    "we will choose the first one (",
                    surv$label[1], ")")
        message(s)
        surv <- surv[1, ]
    }
    setDT(surv)
    D <- as.data.table(data)
    ## must have something to work with
    if(is.null(bnry) & is.null(real) &
       is.null(bnry.manual) & is.null(real.manual)){
        stop("to much null")
    }
    ## force binary data to be 0/1
    bnry01 <- function(x){
        if(length(unique(x[!is.na(x)])) != 2) stop("some 'bnry' not binary")
        as.numeric(as.factor(x))-1
    }
    D[, c(main, bnry) := lapply(.SD, bnry01), .SDcols = c(main, bnry)]
    ## guide to variables
    g0 <- data.table(term = c(main, bnry, real, names(bnry.manual), names(real.manual)),
                     type = c("main",
                              rep("bnry", length(bnry)),
                              rep("real", length(real)),
                              rep("bnry", length(bnry.manual)),
                              rep("real", length(real.manual))),
                     manual = rep(c(0,1), c(length(c(main, bnry, real)),
                                            length(c(bnry.manual, real.manual)))),
                     stringsAsFactors = FALSE)
    ## model formula
    ftxt <- paste0(surv[1, paste0("survival::Surv(", time,
                                  ", ", event, ")")],
                   " ~ ", main,
                   if(!is.null(bnry)) " + " else NULL,
                   paste(bnry, collapse = " + "),
                   if(!is.null(real)) " + " else NULL,
                   paste(real, collapse = " + "),
                   if(!is.null(xtra.adj)) " + " else NULL,
                   xtra.adj)
    M <- survival::coxph(formula(ftxt), data = D)
    sm <- survival:::summary.coxph(M)
    mod <- data.frame(term = dimnames(sm$conf.int)[[1]],
                      adjHR = sm$conf.int[, "exp(coef)"],
                      adjHR.l = sm$conf.int[, "lower .95"],
                      adjHR.u = sm$conf.int[, "upper .95"])
    rownames(mod) <- NULL
    ## get input stats from variables in data set
    if(!is.null(c(bnry, real))){
        stat <- NULL
        for(term in c(bnry, real)){ ## term = c(bnry, real)[1]
            tmp <- D[, .(m = mean(dummy)), by = main, env = list(dummy = term)][
                order(dummy2), env = list(dummy2 = main)]
            s <- dcast(data = cbind(term, tmp),
                       formula = as.formula(paste0("term ~ ", main)),
                       value.var = "m")
            setnames(s, new = c("term", "stat0", "stat1"))
            stat <- rbind(stat, s)
        }
    } else stat <- NULL
    ## get input stats from manually added variables
    manual <- c(bnry.manual, real.manual)
    if(!is.null(manual)){
        man.stat <- data.table(term = names(manual),
                               stat0 = unlist(lapply(manual, function(x) x[2])),
                               stat1 = unlist(lapply(manual, function(x) x[3])))
        man.mod <- data.table(term = names(manual),
                              adjHR = unlist(lapply(manual, function(x) x[1])),
                              adjHR.l = NA,
                              adjHR.u = NA)
    } else{
        man.stat <- man.mod <- NULL
    }
    R <- merge(x = g0,
               y = merge(x = rbind(stat, man.stat),
                         y = rbind(mod, man.mod),
                     by = "term", all = TRUE),
               by = "term", all = TRUE)
    R$stat0[R$term == main] <- 0
    R$stat1[R$term == main] <- 1
    ## determine the changed effect of main when added confounder which is
    ## similar to the already existing covariates in distribution and HR
    new_var <- c("mainHR", "mainHR.l", "mainHR.u",
                 "mainHRinv", "mainHRinv.l", "mainHRinv.u")
    for(term in new_var) R[, dummy := NA_real_, env = list(dummy = term)]
    for(i in 1:nrow(R)){ ## i = 1
        if(is.na(R$type[i]) | !R$type[i] %in% c("bnry", "real")) next
        foo <- function(HR, inv = FALSE){
            update_HR(HR = HR,
                      expG = if(inv) 1/R$adjHR[i] else R$adjHR[i],
                      s0 = R$stat0[i],
                      s1 = R$stat1[i],
                      type = R$type[i])
        }
        est <- M$coefficients[main]
        pm <- (qnorm(.975) * sqrt(diag(M$var)))[1]
        R[i, mainHR := foo(HR = exp(est))]
        R[i, mainHR.l := foo(HR = exp(est - pm))]
        R[i, mainHR.u := foo(HR = exp(est + pm))]
        R[i, mainHRinv := foo(HR = exp(est), inv = TRUE)]
        R[i, mainHRinv.l := foo(HR = exp(est - pm), inv = TRUE)]
        R[i, mainHRinv.u := foo(HR = exp(est + pm), inv = TRUE)]
    }
    attr(R, "formula") <- ftxt
    attr(R, "tidy") <- coxreg_bias_tidy(R)
    R
}

##' bias adjusted HR
##'
##' calculate what treatment HR would be if we could adjust for an unmeasured
##'     binary (continuous) confounder U having proportion (mean) s0 and s1 in
##'     the control- and treatment group, respectively, and whose HR on the
##'     outcome is expG. The formulas are from Lin, D.  Y., Psaty, B.  M., &
##'     Kronmal, R.  A.  (1998).  Assessing the sensitivity of regression
##'     results to unmeasured confounders in observational studies. Biometrics,
##'     948-963.
##' @param HR numeric; the treatment HR (to be updated) on the outcome
##' @param expG numeric; the confounder HR on the outcome
##' @param s0 numeric; proportion/mean among controls
##' @param s1 numeric; proportion/mean among cases
##' @param type character; specification of type of unmeasured confounder,
##'     binary ('bnry') or continuous ('real')
##' @export
update_HR <- function(HR, expG, s0, s1, type = c("bnry", "real")){
    properties(HR, class = "numeric", length = 1, na.ok = FALSE)
    properties(expG, class = "numeric", length = 1, na.ok = FALSE)
    properties(s0, class = "numeric", length = 1, na.ok = FALSE)
    properties(s1, class = "numeric", length = 1, na.ok = FALSE)
    type <- match.arg(type)
    if(type == "bnry"){
        exp(log(HR) - log((expG * s1 + 1 - s1) / (expG * s0 + 1 - s0)))
    } else if(type == "real"){
        exp(log(HR) - log(expG) * (s1 - s0))
    } else {
        stop("This error message should be impossible")
    }
}


#-#' coxreg bias plot data
#-#'
#-#' helper function to get (what I deem) the important parts of the results of
#-#'     \code{coxreg_bias} into a tidy format suitable for plotting
#-#' @param x the return of a \code{coxreg_bias} call
#-#' @return a data frame
coxreg_bias_tidy <- function(x){
    ## get effect of main without U
    A <- subset(x, x$type == "main")
    A$eff <- 'Effect as-is'
    A$alt <- "(No U)"
    A <- A[, c("term", "alt", "eff", "adjHR", "adjHR.l", "adjHR.u")]
    names(A)[4:6] <- c("HR", "ci1", "ci2")
    A2 <- A
    A2$eff <- 'Inverse effect'
    ## get as-is effect of main with U's added
    B <- subset(x, x$type %in% c("bnry", "real"))
    B$eff <- 'Effect as-is'
    B$alt <- paste0("U as ", B$term)
    B <- B[, c("term", "alt", "eff", "mainHR", "mainHR.l", "mainHR.u")]
    names(B)[4:6] <- c("HR", "ci1", "ci2")
    ## get inverse effect of main with U's added
    C <- subset(x, x$type %in% c("bnry", "real"))
    C$eff <- 'Inverse effect'
    C$alt <- paste0("U as ", C$term)
    C <- C[, c("term", "alt", "eff", "mainHRinv", "mainHRinv.l", "mainHRinv.u")]
    names(C)[4:6] <- c("HR", "ci1", "ci2")
    ## rbind
    R <- rbind(A, B, A2, C)
    ## give lists of orders as attribute
    attr(R, "orders") <- list(
        "asis_dec" = B[order(B$HR, decreasing = TRUE), "alt", drop = TRUE],
        "asis_inc" = B[order(B$HR, decreasing = FALSE), "alt", drop = TRUE],
        "inverse_dec" = C[order(C$HR, decreasing = TRUE), "alt", drop = TRUE],
        "inverse_inc" = C[order(C$HR, decreasing = FALSE), "alt", drop = TRUE]
    )
    ## return
    R
}
