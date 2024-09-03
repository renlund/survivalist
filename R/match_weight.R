##' @title matching weights
##' @description Different weightings and some additional details I find useful
##'     in connection with a fullmatch (or similar matching)
##' @param tr the binary treatment variable, as character
##' @param cl the clustering variable, as character
##' @param id the id variable, as character.
##' @param data the data
##' @param trv the value of the treatment (of variable \code{tr}), '1' by
##'     default
##' @return data frame with new parameters \code{n}, number of individuals in
##'     each cluster, \code{tr_n} the number of treated within the cluster,
##'     \code{ctrl_n} the number of controls within the cluster,
##'     \code{ate.weight} the weight for calculating the average treatment
##'     effect, \code{att.weight} the weight for calculating the average
##'     treatment effect for the treated, \code{atc.weight} the weight for
##'     calculating the average treatment effect for the controls, \code{cid}
##'     for describing the match it is useful to have a 'cluster id', use this
##'     with e.g. \code{dplyr::group_by(tr, cid)} and \code{summarise} with
##'     functions using \code{weight = cl.weight} to get stats for weighted
##'     treated and control statistics.
##' @examples
##' df <- data.frame(
##'    id = 1:13,
##'    foo = c(0,0,1, 0,1, 1,1,0, 1,1,0,0, 0),
##'    bar = c(rep(c(letters[1:4]), c(3,2,3,4)), NA),
##'    x = round(runif(13),2)
##')
##' str(match_weight(data = df, tr = "foo", cl = "bar", id = "id"))
##' df$foo <- ifelse(df$foo == 1, "Treated", "Control")
##' str(match_weight(tr = df$foo, cl = df$bar, id = df$id, trv = "Treated"))
##' @export
match_weight <- function(tr, cl, id, data = NULL, trv = 1){
    if(!is.null(data)){
        properties(data, class = "data.frame")
        properties(tr, class = "character", length = 1)
        properties(cl, class = "character", length = 1)
        properties(id, class = "character", length = 1)
        nm <- c(id, tr, cl)
        inclusion(x = names(data), nm = "data", include = nm)
        DATA <- subset(data, select = nm)
        names(DATA) <- c("id", "tr", "cl")
    } else {
        n <- length(id)
        if(length(tr) != n | length(cl) != n){
            stop("tr, cl, id need be of the same length")
        }
        DATA <- data.frame('id' = id, 'tr' = tr, 'cl' = cl,
                        stringsAsFactors = FALSE)
    }
    if(!any(DATA$tr == trv)){
        stop("no occurence of trv in tr")
    }
    na.filt = is.na(DATA$cl)
    Dna <- subset(DATA, subset = na.filt)
    D   <- subset(DATA, subset = !na.filt)
    counter <- function(x){
        n <- nrow(x)
        if(n > 0){
            x$n <- n
            x$tr_n <- sum(x$tr == trv)
            x$ctrl_n <- sum(x$tr != trv)
            x
        } else NULL
    }
    E <- do.call(what = rbind,
                 args = lapply(X = split(D, f = D$cl), FUN = counter))
    E$ate.weight <- with(E,
                         ifelse(tr == trv,
                         (ctrl_n+tr_n)/(2*tr_n),
                         (ctrl_n+tr_n)/(2*ctrl_n)))
    E$att.weight <- with(E, ifelse(tr == trv,
                                   1,
                                   ifelse(tr_n == 1, 1/ctrl_n, tr_n/ctrl_n)))
    E$atc.weight <- with(E, ifelse(tr == trv,
                            ifelse(ctrl_n == 1, 1/tr_n, ctrl_n/tr_n),
                            1))
    E$cid <- with(E, paste0(cl, ":",
                            ifelse(tr == trv, 'tr', 'ctrl'),
                            ifelse(tr == trv, tr_n, ctrl_n)))
    if(nrow(Dna) > 0){
        for(K in setdiff(names(E), names(Dna))){
            Dna[[K]] <- NA
        }
    }
    rbind(E, Dna, make.row.names = FALSE)
}
