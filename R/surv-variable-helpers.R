##' surv variable identifier helpers
##'
##' @param s character; common name of surv variables (differing only in prefix/suffix)
##' @param fix character; a prefix or suffix
##' @param prefix logical; if TRUE indicates prefix is used, if FALSE suffix
##' @name surv-identifiers
NULL

#' @rdname surv-identifiers
#' @details get_or_test_fix: get the default prefix/suffix, or test a candidate
#' @export
get_or_test_fix <- function(fix = NULL){
    if(is.null(fix)) fix = options("survivalist.fix")[[1]]
    properties(fix, class = "character", length = 2, na.ok = FALSE)
    properties(names(fix), nm = "names of fix", class = "character",
               length = 2, na.ok = FALSE)
    inclusion(names(fix), nm = "names of fix", include = c("time", "event"))
    if(names(fix)[1] != "time"){
        stop("first component of 'fix' should be names 'time'")
    }
    fix
}

#' @rdname surv-identifiers
#' @details get_or_test_prefix: get the default logical option for if a prefix
#'     (rather than suffix) should be used, or test a candidate
#' @export
get_or_test_prefix <- function(prefix = NULL){
    if(is.null(prefix)) prefix = options("survivalist.prefix")[[1]]
    properties(prefix, class = "logical", length = 1, na.ok = FALSE)
    prefix
}

#' @rdname surv-identifiers
#' @details surv_paste: paste a surv name and a (pre/suf)fix
#' @export
surv_paste <- function(s, fix){
    properties(s, class = "character", na.ok = FALSE)
    properties(fix, class = "character", length = 1, na.ok = FALSE)
    prefix = get_or_test_prefix()
    if(prefix) paste0(fix, s) else paste0(s, fix)
}

#' @rdname surv-identifiers
#' @details surv_t: create time-component name from a surv name
#' @export
surv_t <- function(s){
    fix <- get_or_test_fix()
    surv_paste(s, fix = fix["time"])
}

#' @rdname surv-identifiers
#' @details surv_e: create event-component name from a surv name
#' @export
surv_e <- function(s){
    fix <- get_or_test_fix()
    surv_paste(s, fix = fix["event"])
}

#' @rdname surv-identifiers
#' @details surv_nm: create component names from a surv name
#' @export
surv_nm <- function(s){
    c(time = surv_t(s), event = surv_e(s))
}

#' @rdname surv-identifiers
#' @details check_stab: check a surv-table ('stab'), i.e. if it is a data.frame
#'     with variables 'label', 'time' and 'event' and, optionally, if the time
#'     and event component specified exist in 'nm' (typically the names of some
#'     data set)
#' @export
check_stab <- function(stab, nm = NULL){
    properties(stab, class = "data.frame")
    inclusion(names(stab), nm = "names of stab",
              include = c("label", "time", "event"))
    if(is.null(nm)){
        stab
    } else {
        lost_times <- which( !(stab$time %in% nm) )
        lost_events <- which( !(stab$events %in% nm) )
        if(length(lost_times) > 0){
            s <- paste0("one or more time components (",
                        paste0(stab$time[lost_times], collapse = ", "),
                        ") are missing\n")
            warning(s)
        }
        if(length(lost_events) > 0){
            s <- paste0("one or more event components (",
                        paste0(stab$time[lost_events], collapse = ", "),
                        ") are missing\n")
            warning(s)
        }
        keep <- setdiff(x = 1:nrow(stab),
                        y = c(lost_times, lost_events))
        stab[keep,]
    }
}


#' @rdname surv-identifiers
#' @details slist_time: extract time component names from surv-list
#' @export
slist_time <- function(sl){
    unlist(lapply(sl, function(x) x[1]))
}

#' @rdname surv-identifiers
#' @details slist_event: extract event component names from surv-list
#' @export
slist_event <- function(sl){
    unlist(lapply(sl, function(x) x[2]))
}

#' @rdname surv-identifiers
#' @details check_slist: check a surv-list; i.e. check if the component names
#' exist in 'nm' (typically the variable names)
#' @export
check_slist <- function(sl, nm){
    N <- length(sl)
    if(N == 0) return(sl)
    sl_nm <- names(sl)
    def_nm <- sprintf("%s", 1:N)
    sl_nm <- if(is.null(sl_nm)){
                def_nm
            } else{
                ifelse(sl_nm == "",
                       yes = def_nm,
                       no = paste0("'", sl_nm, "'"))
            }
    index <- NULL
    for(i in seq_along(sl)){
        x <- sl[[i]]
        ti <- x[1] %in% nm
        ev <- x[2] %in% nm
        if(ti & ev){
            index <- c(index, i)
        } else {
            s <- paste0("entry ", sl_nm[i], ": ",
                        paste(c(if(!ti) paste0("time component '",
                                              x[1], "' not found"),
                                if(!ev) paste0("event component '",
                                               x[2], "' not found")),
                              collapse = " and "))
            warning(s)
        }
    }
    w <- if(is.null(names(sl))) seq_along(sl) else which(names(sl)[index] == "")
    if(length(w) != 0){
        m <- paste0("missing surv name in (input) element ", def_nm[index][w],
                    " has been imputed\n")
        names(sl)[index][w] <- slist_event(sl)[index][w]
        warning(m)
    }
    sl[index]
}

#' @rdname surv-identifiers
#' @details surv_search_string: create the search string to find the time and
#'     event components (if systematically named accoriding to defaults).
#' @export
surv_search_string <- function(){
    prefix = get_or_test_prefix()
    fix = get_or_test_fix()
    f <- gsub(".", "\\.", fix, fixed = TRUE)
    search_t <- surv_paste(s = f['time'],
                           fix = if(prefix) "^" else "$")
    search_e <- surv_paste(s = f['event'],
                           fix = if(prefix) "^" else "$")
    c("time" = search_t, "event" = search_e)
}

#' @rdname surv-identifiers
#' @details create_slist: create surv-list from surv names
#' @export
create_slist <- function(s){
    L <- as.list(NULL)
    s_nm <- names(s)
    for(i in seq_along(s)){
        L[[s[i]]] <- c(surv_t(s[i]), surv_e(s[i]))
    }
    if(!is.null(s_nm)){
        names(L) <- ifelse(s_nm == "", names(L), s_nm)
    }
    L
}

#' @rdname surv-identifiers
#' @details extract_slist_from_names: extract all candidate surv-components from
#' 'nm' (typically the variable names)
#' @export
extract_slist_from_names <- function(nm){
    ss <- surv_search_string()
    cand_t <- grepl(ss['time'], nm)
    ts <- sub(ss['time'], "", nm[cand_t])
    cand_e <- grepl(ss['event'], nm)
    es <- sub(ss['event'], "", nm[cand_e])
    create_slist(intersect(ts, es))
}


if(FALSE){

    nm <- c("ev.foo", "t.foo", "ev.bar", "t.baz", "yo.t.", "lev.t.",
            "quuz_t", "quuz_ev")
    sl <- list(FOO = c("t.foo", "ev.foo"),
               NOPE = c("t.bar", "ev.bar"),
               c("quuz_t", "quuz_ev"))

    ## =========================================================================
    options("survivalist.fix" = c("time" = "t.", "event" = "ev."))
    options("survivalist.prefix" = TRUE)

    get_or_test_fix()
    get_or_test_prefix()
    surv_paste("FOO", "__first_or_last_depending_on_default_value__")
    surv_t("foo")
    surv_e("foo")
    surv_nm("foo")

    slist_time(sl)
    slist_event(sl)
    check_slist(sl, nm)
    surv_search_string()
    create_slist(s = c("foo", "bar"))
    create_slist(s = c(FOO = "foo", BAR = "bar"))
    extract_slist_from_names(nm)

    ## =========================================================================
    options("survivalist.fix" = c("time" = "_t", "event" = "_ev"))
    options("survivalist.prefix" = FALSE)

    get_or_test_fix()
    get_or_test_prefix()
    surv_paste("FOO", "__first_or_last_depending_on_default_value__")
    surv_t("foo")
    surv_e("foo")
    surv_nm("foo")

    slist_time(sl)
    slist_event(sl)
    check_slist(sl, nm)
    surv_search_string()
    create_slist(s = c("foo", "bar"))
    create_slist(s = c(FOO = "foo", BAR = "bar"))
    extract_slist_from_names(nm)

}
