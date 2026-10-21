##' identify withdrawn dispensation
##'
##' Function specific to the data structure of Swedish National Prescribed Drug
##' Register. Within such a data set, this function looks at rows where a
##' dispensation has been withdrawn ('antal < 0') and tries to identify a
##' previous row which matches on the relevant information (id, atc, edatum,
##' antnum, styrkalf), i.e. which previous row that is cancelled by this
##' withdrawal.
##' @param data a data set following the naming conventions of the Swedish
##'     National Prescribed Drug Register
##' @param id character; the name of the subject id variable
##' @param by.ref logical; if data is a data.table, derive variables by
##'     reference?
##' @return The input data frame with the following variables added
##' \itemize{
##'  \item{".rid"}{A row id variable}
##'  \item{".did"}{A duplication id variable. Points to the withdrawn .rid. Rows
##' with non-missing values on this variables are candidates for removal. }
##'  \item{".dd"}{Date difference within the same .did-level. Large values
##' should perhaps be inspected.}
##' }
##' @export
idwidi <- function(data, id = "id", by.ref = NULL){
    ## check arguments
    properties(id, class = "character", length = 1, na.ok = FALSE)
    properties(data, class = "data.frame")
    if(!is.null(by.ref)){
        properties(by.ref, class = "logical", length = 1, na.ok = FALSE)
    }
    ## determine if a copy should be made
    dt <- is.data.table(data)
    if(is.null(by.ref)) by.ref <- dt
    if(by.ref & !dt) by.ref <- FALSE
    d <- if(by.ref) data else as.data.table(data)
    ## lower case names and check that required names exists
    nm <- names(copy(data))
    nm_low <- tolower(nm)
    nm_dup <- nm_low[duplicated(nm_low)]
    if(length(nm_dup) > 0){
        s <- paste0("some 'tolower'-variable names are duplicated ",
                    "{", paste0(nm_dup, collapse = ", "), "}")
    }
    setnames(d, old = nm, new = nm_low)
    d[, .rid := 1:.N]
    term1 <- c(id, "atc", "edatum", "antnum", "styrkalf")
    term2 <- c(term1, "antal", ".rid")
    inclusion(names(d), nm = "(lowercase) names of data", include = term2)
    setorder(d, id, atc, edatum, antnum, styrkalf, -antal)
    ## get to work
    if(d[, any(antal == 0)]){
        message("rows with antal == 0 exists")
    }
    d1 <- d[antal < 0, term2, with = FALSE]
    d1[, antal := -antal]
    setnames(d1, old = ".rid", new = ".did")
    d2 <- d[antal > 0, term2, with = FALSE]
    M <- d2[d1, on = .(id, atc, antnum, styrkalf, antal, edatum), roll = Inf, mult = "last"]
    if(M[, any(.rid == .did)]){
        warning("if you see this - run!")
    }
    d[M, .did := i..did, on = ".rid"]
    d[.rid %in% M$.did, .did := .rid]
    nomatch <- d[antal < 0 & is.na(.did), .rid]
    if(length(nomatch) > 0 ){
        s <- paste0("Withdrawn dispensation not identified at rows ",
                    paste0(sort(nomatch), collapse = ", "),
                    ". (Where 'antal < 0' and 'is.na(.did)')")
        message(s)
    }
    d[!is.na(.did), .dd := as.integer(max(edatum) - min(edatum)), by = .did]
    setorder(d, .rid)
    setnames(d, old = nm_low, new = nm)
    if(by.ref) invisible(d) else if(dt) d else as.data.frame(d)
}
