##' proximity of condition (subset-ish)
##'
##' This function will return lines close to those identified by the condition
##' @param data data.frame
##' @param id character; name of id variable
##' @param ... a condition passed to subset
##' @param prox length 2 non-negative integer vector; how many lines before and
##'     after (respectively) the condition do you want returned
##' @examples
##' d <- data.frame(id = rep(1:3, each = 5),
##'                 foo = LETTERS[1:15],
##'                 bar = rep(letters[5:1], 3))
##' proxcond(d, id = "id", foo=="H")
##' proxcond(d, id = "id", foo=="H", prox = c(Inf,0))
##' proxcond(d, id = "id", foo=="H", prox = c(1,1))
##' proxcond(d, id = "id", foo %in% c("G", "H"), prox = c(1,1))
##' proxcond(d, id = "id", bar == "b", prox = c(0,1))
##' @export
proxcond <- function(data, id, ..., prox = c(Inf,Inf)){
    properties(data, class = "data.frame")
    properties(id, class = "character", length = 1, na.ok = FALSE)
    inclusion(names(data), nm = "names of data", include = id)
    properties(prox, class = c("integer", "numeric"), length = 2, na.ok = FALSE)
    if(prox[1] < 0 | prox[2] < 0){
        stop("argument prox should be non-nagative integers")
    }
    return_dt <- return_data.table(is.data.table(data))
    dt <- as.data.table(data)
    setnames(dt, old = id, new = ".id")
    dt[, `:=`(.row = 1:.N, .rowid = rowid(.id))]
    dt[, .maxrowid := max(.rowid), by = .id]
    I <- dt[..., c(".id", ".row", ".rowid", ".maxrowid")][
      , `:=`(.lo = pmax(.rowid - prox[1], 1),
             .hi = pmin(.rowid + prox[2], .maxrowid))]
    if(nrow(I) > 0){
        expander <- function(id,a,b) data.frame(.id=id,.rowid=a:b)
        I2 <- I[, expander(.id,.lo,.hi), by=.row][
            !duplicated(data.table(.id,.rowid))][
          , .row := NULL]
        R <- dt[I2, on = .(.id, .rowid)]
    } else R <- dt[rep(FALSE, .N)]
    R[, c(".row", ".rowid", ".maxrowid") := NULL]
    setnames(R, old = ".id", new = id)
    if(return_dt) R else as.data.frame(R)
}
