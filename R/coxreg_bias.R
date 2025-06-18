# NOTE: these functions imported from earlier package sensias. Could use an oversight.

##' Bias analysis for cox regression
##'
##' Get estimates of 'true' effect given the addition of a confounder, as
##' calculated by \code{update_HR}.
##' @param data data frame
##' @param surv name of response 'Surv'-variable in data set OR the name of the
##'     time- and status component of a time-to-event variable
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
##' Also, there is an attribute 'tidy' with easy to plot information.
##' @export
coxreg_bias <- function(data, surv, main,
                        bnry = NULL, real = NULL, xtra.adj = NULL,
                        bnry.manual = NULL, real.manual = NULL){
    properties(data, class = "data.frame")
    properties(surv, class = "character", length = 1:2, na.ok = FALSE)
    properties(main, class = "character", length = 1, na.ok = FALSE)
    if(!is.null(bnry)) properties(bnry, class = "character", na.ok = FALSE)
    if(!is.null(real)) properties(real, class = "character", na.ok = FALSE)
    if(!is.null(xtra.adj)) properties(xtra.adj, class = "character", na.ok = FALSE)
    if(!is.null(bnry.manual)) properties(bnry.manual, class = "list")
    if(!is.null(real.manual)) properties(real.manual, class = "list")
    ## must have something to work with
    if(is.null(bnry) & is.null(real) &
       is.null(bnry.manual) & is.null(real.manual)){
        stop("to much null")
    }
    ## have data in data.frame format, so that it can keep a Surv object
    data <- as.data.frame(data, stringsAsFactors = FALSE)
    ## check surv argument
    if(length(surv) == 1){
        surv.name <- surv
        if(class(data[[surv.name]]) != 'Surv'){
            stop("'surv' is not a 'Surv' object in data")
        }
    } else if(length(surv) == 2){
        data$outcome <- survival::Surv(time = surv[1], event = surv[2])
        surv.name <- "outcome"
        bnry <- setdiff(bnry, "outcome")
        real <- setdiff(real, "outcome")
    } else stop("'surv' argument in strange form")
    ## limit data to what is needed ONLY if xtra.adj is NULL
    D <- if(is.null(xtra.adj)){
             subset(data, subset = TRUE,
                    select = c(surv.name, main, bnry, real))
         } else data
    ## force binary data to be 0/1
    bnry01 <- function(x){
        if(length(unique(x[!is.na(x)])) != 2) stop("some 'bnry' not binary")
        as.numeric(as.factor(x))-1
    }
    D[, c(main, bnry)] <- lapply(D[, c(main, bnry)], bnry01)
    ## guide to variables
    g0 <- data.frame(term = c(main, bnry, real, names(bnry.manual), names(real.manual)),
                     type = c("main",
                              rep("bnry", length(bnry)),
                              rep("real", length(real)),
                              rep("bnry", length(bnry.manual)),
                              rep("real", length(real.manual))),
                     manual = rep(c(0,1), c(length(c(main, bnry, real)),
                                            length(c(bnry.manual, real.manual)))),
                     stringsAsFactors = FALSE)
    ## model formula
    ftxt <- paste0(surv, " ~ ", main,
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
        br_data <- D[, c(bnry, real)]
        lmean <- function(X) unlist(lapply(X, mean, na.rm = TRUE))
        tmp <- split(br_data, f = D[[main]])
        d <- as.data.frame(lapply(tmp, lmean), check.names = FALSE)
        names(d) <- paste0('stat', names(d))
        d$term <- rownames(d)
        stat <- d[, c('term', 'stat0', 'stat1')]
    } else stat <- NULL
    ## get input stats from manually added variables
    manual <- c(bnry.manual, real.manual)
    if(!is.null(manual)){
        man.stat <- data.frame(term = names(manual),
                               stat0 = unlist(lapply(manual, function(x) x[2])),
                               stat1 = unlist(lapply(manual, function(x) x[3])))
        man.mod <- data.frame(term = names(manual),
                              adjHR = unlist(lapply(manual, function(x) x[1])),
                              adjHR.l = NA,
                              adjHR.u = NA)
    } else{
        man.stat <- man.mod <- NULL
    }
    R <- merge(g0, merge(rbind(stat, man.stat), rbind(mod, man.mod),
                         by = "term", all = TRUE), by = "term", all = TRUE)
    ## determine the changed effect of main when added confounder which is
    ## similar to the already existing covariates in distribution and HR
    R$mainHRinv.u <- R$mainHRinv.l <- R$mainHRinv <-
        R$mainHR.u <- R$mainHR.l <- R$mainHR <- rep(NA, nrow(R))
    for(i in 1:nrow(R)){ ## i = 2
        if(is.na(R$type[i]) | !R$type[i] %in% c("bnry", "real")) next
        if(R$type[i] == "bnry"){
            foo <- function(HR, inv = FALSE){
                update_HR(HR = HR,
                          expG = if(inv) 1/R$adjHR[i] else R$adjHR[i],
                          s0 = R$stat0[i],
                          s1 = R$stat1[i],
                          type = 'bnry')
            }
            est <- M$coefficients[main]
            pm <- (qnorm(.975) * sqrt(diag(M$var)))[1]
            R$mainHR[i] <- foo(HR = exp(est))
            R$mainHR.l[i] <- foo(HR = exp(est - pm))
            R$mainHR.u[i] <- foo(HR = exp(est + pm))
            R$mainHRinv[i] <- foo(HR = exp(est), inv = TRUE)
            R$mainHRinv.l[i] <- foo(HR = exp(est - pm), inv = TRUE)
            R$mainHRinv.u[i] <- foo(HR = exp(est + pm), inv = TRUE)
        }
        if(R$type[i] == "real"){
            foo <- function(HR, inv = FALSE){
                update_HR(HR = HR,
                          expG = if(inv) 1/R$adjHR[i] else R$adjHR[i],
                          s0 = R$stat0[i],
                          s1 = R$stat1[i],
                          type = 'real')
            }
            est <- M$coefficients[main]
            pm <- (qnorm(.975) * sqrt(diag(M$var)))[1]
            R$mainHR[i] <- foo(HR = exp(est))
            R$mainHR.l[i] <- foo(HR = exp(est - pm))
            R$mainHR.u[i] <- foo(HR = exp(est + pm))
            R$mainHRinv[i] <- foo(HR = exp(est), inv = TRUE)
            R$mainHRinv.l[i] <- foo(HR = exp(est - pm), inv = TRUE)
            R$mainHRinv.u[i] <- foo(HR = exp(est + pm), inv = TRUE)
        }
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
    R <- rbind(A, B, C)
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
