##' create indicators for state change
##'
##' For each state in 's', create indicator variables for the state
##' changes. This might be useful e.g. when setting up data for a multistate
##' model.
##' @param s a factor of states
##' @return a data frame with indicators (named as the levels of s) for state
##'     changes
##' @examples
##' d <- data.frame(id=1, tstart = 0:7, tstop = 1:8,
##'                 state = factor(LETTERS[c(1,1,2,1,1,3,2,2)],
##'                                levels = LETTERS[4:1]))
##' cbind(d, state2event(d$state))
##' @export
state2event <- function(s){
    properties(s, class = c("character", "factor"), na.ok = FALSE)
    if(!is.factor(s)) s <- factor(s)
    lev <- levels(s)
    if(length(lev) == 0) stop("'s' has no levels")
    mall <- rep(0L, length(s))
    null.list <- as.list(NULL)
    ut <- lapply(lev, function(z){ null.list[[z]] <- mall})
    names(ut) <- levels(s)
    r <- rle(as.character(s))
    v <- r$values
    R <- if(length(v) == 1){
        as.data.frame(ut, check.names = FALSE)
    } else{
        l <- r$lengths
        n <- length(l)
        lc <- stats::setNames(object = cumsum(l)[-n], nm = v[-1])
        for(i in lev){
            index <- which(names(lc) == i)
            ut[[i]][lc[index]] <- 1L
        }
        as.data.frame(ut, check.names = FALSE)
    }
    return_dt <- return_data.table()
    if(return_dt) R else as.data.frame(R)
}
