##' treatment HR changes
##'
##' examine  changes in  main effect  (HR, Cox  regression) when  covariates are
##'     added one at a time; in solation (both included from minimal model and
##'     excluded from full model), sequentially increasing (starting from
##'     a small model), or sequentially decreasing (starting from large model)
##' @param data data frame
##' @param surv an 'stab' with 1 row or, if surv pair can be identified through
##'     the default affix, the common name (label) for the pair
##' @param main name of main effect (binary)
##' @param terms vector of terms to adjust for (can be named)
##' @param uni logical; include each term's 'univariate' effect on main?
##' @param full logical; include each term's 'exclusion from full model'-effect
##'     on main?
##' @param inc logical; include each  term's effect, by sequential inclusion, on
##'     main?
##' @param exc logical; include each  term's effect, by sequential exclusion, on
##'     main?
##' @param decr.inc logical; decreasing order for sequential inclusion?
##' @param decr.exc logical; decreasing order for sequential exclusion?
##' @return a list of data frames
##' @export
coxreg_change <- function(data, surv = NULL, main, terms,
                          uni = TRUE, full = TRUE,
                          inc = FALSE, exc = FALSE,
                          decr.inc = NULL, decr.exc = NULL){
    properties(data, class = "data.frame")
    properties(surv, class = c("NULL", "character", "data.frame"))
    properties(main, class = "character", length = 1, na.ok = FALSE)
    properties(terms, class = "character", na.ok = FALSE)
    properties(uni, class = "logical", length = 1, na.ok = FALSE)
    properties(full, class = "logical", length = 1, na.ok = FALSE)
    properties(inc, class = "logical", length = 1, na.ok = FALSE)
    properties(exc, class = "logical", length = 1, na.ok = FALSE)
    if(!is.null(decr.inc)){
        properties(decr.inc, class = "logical", length = 1, na.ok = FALSE)
    }
    if(!is.null(decr.exc)){
        properties(decr.exc, class = "logical", length = 1, na.ok = FALSE)
    }
    ## check if terms have names
    if(is.null(names(terms))) names(terms) <- terms
    ## check surv
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
    return_dt <- return_data.table(is.data.table(data))
    data <- as.data.table(data)

    ## model formula
    ffnc <- function(ts = NULL){
        paste0(surv[1, paste0("survival::Surv(", time,
                              ", ", event, ")")],
               " ~ ", main,
               if(length(ts) != 0) " + " else NULL,
               paste0(ts, collapse = " + "))
    }
    Sf <- ffnc()
    Lf <- ffnc(terms)
    Smod <- survival::coxph(formula = formula(Sf), data = data)
    Lmod <- survival::coxph(formula = formula(Lf), data = data)
    ## update function
    upd <- function(f, s, op = " + ") paste0(f, op, s)
    ## get unadjusted effect of main
    est <- Smod$coefficients[1]
    se <-  sqrt(Smod$var[1,1])
    UNADJ <- data.frame(term = "(none)",
                        HR = exp(est),
                        HR.l = exp(est - 1.96*se),
                        HR.h = exp(est + 1.96*se))
    ## get adjusted effect of main
    est <- Lmod$coefficients[1]
    se <-  sqrt(Lmod$var[1,1])
    ADJ <- data.frame(term = "(none)",
                      HR = exp(est),
                      HR.l = exp(est - 1.96*se),
                      HR.h = exp(est + 1.96*se))
    ## get main + univariate adjustments HRs
    if(uni){
        UNI <- UNADJ
        for(i in seq_along(terms)){ ## i = 1
            mtmp <- survival::coxph(formula = formula(upd(Sf, terms[i])), data = data)
            est <- mtmp$coefficients[1]
            se <-  sqrt(mtmp$var[1,1])
            df <- data.frame(term = names(terms)[i],
                             HR = exp(est),
                             HR.l = exp(est - 1.96*se),
                             HR.h = exp(est + 1.96*se))
            UNI <- if(is.null(UNI)) df else rbind(UNI, df)
        }
        rownames(UNI) <- NULL
    }
    ## get full - univariate adjustment HRs
    if(full){
        FULL <- ADJ
        for(i in seq_along(terms)){ ## i = 1
            but1 <- setdiff(terms, terms[i])
            mtmp <- survival::coxph(formula = formula(ffnc(but1)), data = data)
            est <- mtmp$coefficients[1]
            se <-  sqrt(mtmp$var[1,1])
            df <- data.frame(term = names(terms)[i],
                             HR = exp(est),
                             HR.l = exp(est - 1.96*se),
                             HR.h = exp(est + 1.96*se))
            FULL <- if(is.null(FULL)) df else rbind(FULL, df)
        }
        rownames(FULL) <- NULL
    }
    ## determine decr-parameters
    if(is.null(decr.inc) & is.null(decr.exc)){
        decr.inc <- UNADJ$HR < ADJ$HR
        decr.exc <- !decr.inc
    }
    if(inc){
        ## get HR from model of sequential inclusion
        INC <- UNADJ
        if(is.null(decr.inc)) if(INC$HR >= 1){
                                  decr.inc <- FALSE
                              } else {
                                  decr.inc <- TRUE
                              }
        f <- Sf
        curr.terms <- terms
        for(dummy in seq_along(terms)){ ## dummy = 1
            X <- NULL
            for(i in seq_along(curr.terms)){ ## i =1
                mtmp <- survival::coxph(formula = formula(upd(f, curr.terms[i], "+")),
                                 data = data)
                est <- mtmp$coefficients[1]
                se <-  sqrt(mtmp$var[1,1])
                df <- data.frame(term = names(curr.terms)[i],
                                 HR = exp(est),
                             HR.l = exp(est - 1.96*se),
                             HR.h = exp(est + 1.96*se))
                X <- if(is.null(X)) df else rbind(X, df)
            }
            indx <- order(X$HR, decreasing = decr.inc)
            X <- X[indx, ]
            val <- curr.terms[indx][1]
            INC <- if(is.null(INC)) X[1,] else rbind(INC, X[1,])
            curr.terms <- setNames(setdiff(curr.terms, val),
                                   nm = setdiff(names(curr.terms), names(val)))
            f <- upd(f, val)
    }
    rownames(INC) <- NULL
    }
    if(exc){
        ## get HR from model of sequential exclusion
        EXC <- ADJ
        if(is.null(decr.exc)) if(EXC$HR >= 1){
                                  decr.exc <- FALSE
                              } else {
                                  decr.exc <- TRUE
                              }
        f <- Lf
        curr.terms <- terms
        for(dummy in seq_along(terms)){ ## dummy = 1
            X <- NULL
            for(i in seq_along(curr.terms)){ ## i =1
                tmp.terms <- setdiff(curr.terms, curr.terms[i])
                mtmp <- survival::coxph(formula = formula(ffnc(tmp.terms)),
                                 data = data)
                est <- mtmp$coefficients[1]
                se <-  sqrt(mtmp$var[1,1])
                df <- data.frame(term = names(curr.terms)[i],
                                 HR = exp(est),
                                 HR.l = exp(est - 1.96*se),
                                 HR.h = exp(est + 1.96*se))
                X <- if(is.null(X)) df else rbind(X, df)
            }
            indx <- order(X$HR, decreasing = decr.exc)
            X <- X[indx, ]
            val <- curr.terms[indx][1]
            EXC <- if(is.null(EXC)) X[1,] else rbind(EXC, X[1,])
            curr.terms <- setNames(setdiff(curr.terms, val),
                                   nm = setdiff(names(curr.terms), names(val)))
        }
        rownames(EXC) <- NULL
    }
    ## return list of results
    ## L <- as.list(NULL)
    ## if(uni) L$univariate <- UNI
    ## if(full) L$full <- FULL
    ## if(inc) L$sequential_inclusion <- INC
    ## if(exc) L$sequential_exclusion <- EXC
    ## L
    L <- NULL
    if(uni){
        setDT(UNI)
        L <- rbind(L, UNI[, change := "univariate"])
    }
    if(full){
        setDT(FULL)
        L <- rbind(L, FULL[, change := "full"])
    }
    if(inc){
        setDT(INC)
        L <- rbind(L, INC[, change := "sequential_inclusion"])
    }
    if(exc){
        setDT(EXC)
        L <- rbind(L, EXC[, change := "sequential_exclusion"])
    }
    if(return_dt) L[] else as.data.frame(L)
}
