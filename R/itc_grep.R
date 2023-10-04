##' @title individual time constrained grep
##' @description in a dataset with one or more variables (typically containing
##'     text) associated with a date, find matches on those variables for
##'     specific individuals within specifed time frames
##' @param pattern a vector of search strings (regular expressions) (the names
##'     attribute will be used as alias if it exists)
##' @param x names of variables to search in (given in order of importance)
##' @param data a data frame
##' @param id name of id variable (in 'data')
##' @param date name of associated date variable (in 'data')
##' @param units a data frame containing id's as well as (but optionally)
##'     'begin' and 'end' variables
##' @param units.id variable name in 'units' to use as id (by default the same
##'     as 'id') N.B. a unit can appear several times, and will be identified
##'     alongside 'begin' and 'end' (a soft warning will be given if these 3
##'     variables are not enough for uniqueness)
##' @param begin variable name in 'units' to use as begin, if missing will be
##'     set to earliest date in data
##' @param end variable name in 'units' to use as end, if missing will be set to
##'     latest date in data
##' @param include length 2 logical vector specifying if lower (first entry) and
##'     upper (second entry) bounds are inclusive (\code{TRUE}) or not
##'     (\code{FALSE})
##' @param ... arguments passed to \code{data.table::like} for identifying matches
##' @param data.keep character vector of variables you want to keep from 'data'
##' @param verbose if \code{TRUE} the function will give helpful and/or annoying
##'     messages
##' @import data.table
##' @return A data frame with
##'  \itemize{
##'    \item id the id variable
##'    \item alias the name of pattern searched for (else p1, p2, etc)
##'    \item date the date of assicated match
##'    \item time days from 'begin' to 'date'
##'    \item event indicator for a match
##'    \item begin the begin date (could be individual)
##'    \item end the end date (could be individual)
##'    \item match.in the variable the match was found in
##'    \item match the match found
##'    \item first.id indicator for first occurence of associated
##'        id/begin/end-combination
##'    \item first.id_date indicator for first occurence of associated
##'        id/begin/end- AND date combination
##'    \item pattern the pattern searched for
##'    \item ... variables selected with 'data.keep'.
##' }
##'
##' Note that any individual can have more than one match. See the vignette for
##' examples.
##' @export
itc_grep <- function(pattern, x, data, id, date,
                 units, units.id = id, begin = NULL, end = NULL,
                 include = c(TRUE, TRUE), ...,
                 data.keep = NULL, verbose = TRUE){
    ## CHECK ARGUMENTS ---------------------------------------------------------
    properties(x = verbose, class = "logical", length = 1, na.ok = FALSE)
    V <- verbose
    if(V) message("[itc_grep is verbose] Checking arguments")
    properties(x = pattern, class = "character", na.ok =FALSE)
    properties(x = names(pattern), nm = "names of 'pattern'",
               class = c("NULL", "character"), na.ok =FALSE)
    if(is.null(names(pattern))){
        alias <- sprintf("alias %s", 1:length(pattern))
        pattern <- setNames(object = pattern,
                            nm = alias)
        if(V) message("pattern doesn't have names, alias(es) {",
                     paste(alias, collapse = ", "),"} is/are used")
    }
    properties(x = x, class = "character", na.ok = FALSE)
    properties(x = data, class = "data.frame")
    properties(x = id, class = "character", length = 1, na.ok = FALSE)
    properties(x = date, class = "character", length = 1, na.ok = FALSE)
    properties(x = units, class = "data.frame")
    properties(x = units.id, class = "character", length = 1, na.ok = FALSE)
    properties(x = begin, class = c("NULL", "character"),
               length = 0:1, na.ok = FALSE)
    properties(x = end, class = c("NULL", "character"),
               length = 0:1, na.ok = FALSE)
    properties(x = include, class = "logical", length = 2, na.ok = FALSE)
    properties(x = data.keep, class = c("NULL", "character"), na.ok = FALSE)
    inclusion(x = names(data), nm = "'data'",
              include = c(id, date, x, data.keep))
    inclusion(x = names(units), nm = "'units'", include = c(units.id, begin, end))
    avoidance(x = x, avoid = c("id", "date"))
    return_dt <- return_data.table(is.data.table(data))

    ## GET DATA READY ----------------------------------------------------------
    if(V) message("Preparing the data")
    rk <- renaming_key(x = c(data.keep), nm = "'data'",
                       avoid = c("id", "date"))
    data.select <- c(id, date, x, data.keep)
    DATA <- as.data.table(data)[, data.select, with = FALSE]
    setnames(DATA, old = c(id, date, names(rk)), new = c("id", "date", rk))
    setkey(DATA, id, date)

    ## GET UNITS READY ---------------------------------------------------------
    units.select <- c(units.id, begin, end)
    UNITS <- as.data.table(units)[, units.select, with = FALSE]
    setnames(UNITS, old = units.id, new = "id")
    setkey(UNITS, id)
    if(is.null(begin)){
        UNITS[, begin := DATA[, min(date, na.rm = TRUE)]]
    } else setnames(UNITS, old = begin, new = "begin")
    if(is.null(end)){
        UNITS[, end := DATA[, max(date, na.rm = TRUE)]]
    } else setnames(UNITS, old = end, new = "end")
    if(anyDuplicated(UNITS)){
        s <- paste0(" | *soft warning*: units {id, begin, end} contains duplicates")
        warning(s)
    }
    ## this seems like inelegant coding, but I don't know how to best keep
    ##   at copy of 'begin' and 'end' after the non-equi join creating X:
    UNITS[, `:=`(start = identity(begin), stop = identity(end))]
    ## this seems cumbersome, but no better solution as of yet:
    X <- if(include[1]){
             if(include[2]){
                 UNITS[DATA, on = .(id, start <= date, stop >= date),
                       nomatch = NULL]
             } else {
                 UNITS[DATA, on = .(id, start <= date, stop > date),
                       nomatch = NULL]
             }
         } else {
             if(include[2]){
                 UNITS[DATA, on = .(id, start < date, stop >= date),
                       nomatch = NULL]
             } else {
                 UNITS[DATA, on = .(id, start < date, stop > date),
                       nomatch = NULL]
             }
         }
    X[, stop := NULL]
    setnames(X, old = "start", new = "date")

    ## NEED TO STORE A MISSING VERSION OF 'data.keep'-VARIABLES ----------------
    ##   (when filling in OTHER below, else rbindlist might fail due to wrong
    ##      class)
    keepNA <- DATA[1, data.keep, with = FALSE]
    keepNA[1] <- NA

    ## GO THROUGH ALL PATTERNS AND SEARCH VARIABLES ----------------------------
    L <- as.list(NULL)
    for(j in seq_along(pattern)){
        if(V) message("Now searching for ", names(pattern)[j], " (",
                      j, "/", length(pattern), ") in variable:", sep = "")
        ## GET MATCHES FOR PATTERN IN ALL 'x'-VARIABLES - - - - - - - - - - - -
        LL <- as.list(NULL)
        for(i in seq_along(x)){
            if(V) cat(" * [    ] ", x[i], sep = "")
            TMP <- X[like(vector = eval(parse(text = x[i])),
                          pattern = pattern[j])] ## , ...]
            if(length(x) > 1) TMP[, setdiff(x, x[i]) := NULL]
            TMP[, `:=`(event = 1,
                       time = as.numeric(date-begin),
                       match.in = factor(x[i], levels = x))]
            setnames(TMP, old = x[i], new = "match")
            LL[[i]] <- TMP
            if(V) cat("\r * [done] ", x[i], "\n", sep = "")
        }
        MATCH <- rbindlist(LL)
        ## ADD THOSE WITHOUT MATCH USING AN ANTI-JOIN - - - - - - - - - - - - -
        OTHER <- UNITS[!MATCH, on = .(id, begin, end),
                       .(id, begin, end, date = end, match = NA_character_)]
        if(!is.null(data.keep)) OTHER[, (data.keep):= keepNA]
        OTHER[, `:=`(event = 0,
                     time = as.numeric(date-begin),
                     match.in = NA_character_)]
        L[[j]] <- rbind(MATCH, OTHER)[order(id)][
          , `:=`(alias = names(pattern[j]))
        ]
    }

    ## FIX OUTPUT --------------------------------------------------------------
    if(V) message("Pattern search complete. Preparing output")
    R <- rbindlist(L)[order(alias, id, begin, date, match.in)][
      , `:=`(first.id = as.integer(rowid(alias, id, begin, end) == 1),
             first.id_date = as.integer(rowid(alias, id, begin, end, date) == 1))
    ]
    nams <- c("id", "alias", "date", "time", "event", "begin", "end",
              "match.in", "match", "first.id", "first.id_date")
    setcolorder(R, neworder = c(nams, data.keep))
    setattr(R, name = "pattern", value = pattern)
    if(return_dt) R else as.data.frame(R)
}



##' widen itc_grep result
##'
##' Apply \code{data.table::dcast} to turn the result from itc_grep into wide format
##' (one row per {id, begin, end}) with time and event component for each alias.
##' @param x the return object from \code{itc_grep}
##' @param event.only logical; event only data, else time and event components
##'     for each alias
##' @return Wide form of itc_grep. If \code{event.only=FALSE} each alias has the
##'     match associated with \code{first.id == 1} turned into variables
##'     'ev.alias' (value of 'event') and 't.alias' (value of time). If
##'     \code{event.only=FALSE}, each alias gets a column each with the value of
##'     'event' associated with \code{first.id == 1}
##' @export
itc_grep2wide <- function(x, event.only = FALSE){
    properties(x, class = "data.frame")
    properties(event.only, class = "logical", length = 1, na.ok = FALSE)
    inclusion(names(x), nm = "x",
              include = c("id", "begin", "end", "time",
                          "event", "alias", "first.id"))
    return_dt <- return_data.table(is.data.table(x))
    if(!"data.table" %in% class(x)) x <- as.data.table(x)
    a <- unique(x[['alias']])
    R <- if(event.only){
             dcast(data = x[first.id == 1,
                            .(id, begin, end, event, alias)],
                   formula = id + begin + end ~ alias,
                   value.var = c("event"))
         } else {
             dc = dcast(data = x[first.id == 1,
                                 .(id, begin, end, time, event, alias)],
                        formula = id + begin + end ~ alias,
                        value.var = c("time", "event"))
             g1 <- gsub(pattern = "time_", replacement = "t.",
                        x = names(dc), fixed = TRUE)
             g2 <- gsub(pattern = "event_", replacement = "ev.",
                        x = g1, fixed = TRUE)
             setnames(dc, old = names(dc), new = g2)
             colorder <- c("id", "begin", "end",
                           paste0(c("t.", "ev."), rep(a, each = 2)))
             setcolorder(dc, neworder = colorder)
         }
    if(return_dt) R else as.data.frame(R)
}
