##' survival table (stab) functions
##'
##' @param s character; common name of surv variables (differing only in
##'     prefix/suffix)
##' @param fix character; a prefix or suffix
##' @param prefix logical; if TRUE indicates that a prefix is used, if FALSE
##'     suffix
##' @param stab data.frame, a "survival table" (stab) i.e. a data frame
##'     specifying the surv-pair variables. Must contain columns 'label', 'time'
##'     and 'event'. (Optionally it could contain 'group', especially if one
##'     might want to merge this information with a "variable table" (vtab).)
##' @param sl list; a "survival list" (slist) - an older type of specification
##'     of surv-pair variables not in use
##' @param term character; variable name(s)
##' @param nm character vector; typically the names of variables in a data set
##' @param vtab data.frame; a "variable table" (vtab)
##' @param group.name character; name of grouping for surv variables
##' @name stab-fncs
NULL

#' @rdname stab-fncs
#' @details default.fix: get default value of 'fix'
#' @export
default.fix <- function() options("survivalist.fix")[[1]]

#' @rdname stab-fncs
#' @details default.prefix: get the default prefix/suffix
#' @export
default.prefix <- function() options("survivalist.prefix")[[1]]

#' @rdname stab-fncs
#' @details default.surv.group.name: get the default group name for survival
#'     variables
#' @export
default.surv.group.name <- function() options("survivalist.surv.group.name")[[1]]

#' @rdname stab-fncs
#' @details get_or_test_fix: test (and get default if missing) value of 'prefix'
#' @export
get_or_test_fix <- function(fix = NULL){
    if(is.null(fix)) fix = default.fix()
    properties(fix, class = "character", length = 2, na.ok = FALSE)
    properties(names(fix), nm = "names of fix", class = "character",
               length = 2, na.ok = FALSE)
    inclusion(names(fix), nm = "names of fix", include = c("time", "event"))
    if(names(fix)[1] != "time"){
        stop("first component of 'fix' should be named 'time'")
    }
    fix
}

#' @rdname stab-fncs
#' @details get_or_test_prefix: test (and get the default if missing) the
#'     logical option for if a prefix (rather than suffix) should be used
#' @export
get_or_test_prefix <- function(prefix = NULL){
    if(is.null(prefix)) prefix = default.prefix()
    properties(prefix, class = "logical", length = 1, na.ok = FALSE)
    prefix
}

#' @rdname stab-fncs
#' @details surv_paste: paste a surv name and a (pre/suf)fix
#' @export
surv_paste <- function(s, fix, prefix = NULL){
    properties(s, class = "character", na.ok = FALSE)
    properties(fix, class = "character", length = 1, na.ok = FALSE)
    prefix <- get_or_test_prefix(prefix)
    if(prefix) paste0(fix, s) else paste0(s, fix)
}

#' @rdname stab-fncs
#' @details surv_t: create time-component name from a surv name
#' @export
surv_t <- function(s, fix = NULL, prefix = NULL){
    if(length(s) == 0){
        character(0)
    } else {
        fix <- get_or_test_fix(fix)
        surv_paste(s, fix = fix["time"], prefix = prefix)
    }
}

#' @rdname stab-fncs
#' @details surv_e: create event-component name from a surv name
#' @export
surv_e <- function(s, fix = NULL, prefix = NULL){
    if(length(s) == 0){
        character(0)
    } else {
        fix <- get_or_test_fix(fix)
        surv_paste(s, fix = fix["event"], prefix = prefix)
    }
}

#' @rdname stab-fncs
#' @details surv_nm: create component names from a surv name
#' @export
surv_nm <- function(s, fix = NULL, prefix = NULL){
    properties(s, class = "character", length = 1, na.ok = FALSE)
    c("time" = surv_t(s, fix = fix, prefix = prefix),
      "event" = surv_e(s, fix = fix, prefix = prefix))
}

#' @rdname stab-fncs
#' @details check_stab: check that correct variables are included
check_stab <- function(stab){
    properties(stab, class = "data.frame")
    inclusion(names(stab), nm = "names of surv table",
              include = c("label", "time", "event"))
    n <- nrow(stab)
    if(n > 0){
        properties(stab$label, class = "character", length = n, na.ok = FALSE)
        properties(stab$time, class = "character", length = n, na.ok = FALSE)
        properties(stab$event, class = "character", length = n, na.ok = FALSE)
    } else {
        warning("surv table has zero rows")
    }
    stab
}

##' @rdname stab-fncs
##' @details which_surv_component: is 'term' the time- event component of a
##'     surv-variable pair (if not: NA_character_)
##' @export
which_surv_component <- function(term, stab){
    if(is.null(stab)){
        rep(NA_character_, length(term))
    } else {
        ifelse(test = term %in% stab$time,
               yes = "time",
               no = ifelse(test = term %in% stab$event,
                           yes = "event",
                           no = NA_character_))
    }
}

##' @rdname stab-fncs
##' @details is_surv_component: is 'term' a component of a surv-variable pair
##' @export
is_surv_component <- function(term, stab){
    !is.na(which_surv_component(term = term, stab = stab))
}

#' @rdname stab-fncs
#' @details verify_stab: check a surv-table; i.e. check if the component names
#'     exist in 'nm' (typically the variable names). Return an stab reduced to
#'     those components that exist in nm.
#' @export
verify_stab <- function(stab, nm, warn = TRUE){
    check_stab(stab)
    properties(nm, class = "character", na.ok = FALSE)
    properties(warn, class = "logical", length = 1, na.ok = FALSE)
    i_time  <- stab$time %in% nm
    i_event <- stab$event %in% nm
    i <- i_time & i_event
    if(warn && any(!i)){
        s1 <- paste0("time components missing for: ",
                     paste0(stab$label[!i_time], collapse = ", "), ".")
        s2 <- paste0("event components missing for: ",
                     paste0(stab$label[!i_event], collapse = ", "), ".")
        s <- paste0(if( any(!i_time) ) paste0(s1, "\n") else NULL,
                    if( any(!i_event) ) s2 else NULL)
        warning(s)
    }
    stab[i,]
}

#' @rdname stab-fncs
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

#' @rdname stab-fncs
#' @details create_stab: create survival table from surv names. Use name attribute
#'     of s as labels if such exist.
#' @export
create_stab <- function(s){
    properties(s, class = "character", na.ok = FALSE)
    nm <- names(s)
    if(!is.null(nm)) properties(nm, class = "character", na.ok = FALSE)
    data.frame(label = if(!is.null(nm)) nm else s,
               time = surv_t(s),
               event = surv_e(s))
}

#' @rdname stab-fncs
#' @details extract_stab_from_names: extract all candidate surv-components from
#'     'nm' (typically the variable names)
#' @export
extract_stab_from_names <- function(nm){
    ss <- surv_search_string()
    cand_t <- grepl(ss['time'], nm)
    ts <- sub(ss['time'], "", nm[cand_t])
    cand_e <- grepl(ss['event'], nm)
    es <- sub(ss['event'], "", nm[cand_e])
    r <- create_stab(intersect(ts, es))
    if(nrow(r) > 0) r else NULL
}

#' @rdname stab-fncs
#' @details stab2vtab: convert survival table to variable table
#' @export
stab2vtab <- function(stab, group.name = NULL){
    check_stab(stab)
    if(is.null(group.name)){
        group.name = default.surv.group.name()
    }
    properties(group.name, class = "character",
               length = 1, na.ok = FALSE)
    if(is.null(stab$group)){
        stab$group <- group.name
    }
    stab$label.time <- paste0(stab$label, " (time)")
    nm <- c("term", "label", "group")
    a <- stab[, c("event", "label", "group")]
    names(a) <- nm
    b <- stab[, c("time", "label.time", "group")]
    names(b) <- nm
    r <- rbind(a,b)
    n <- nrow(r)
    i <- shuffle(1:(n/2), (n/2+1):n)
    r[i, ]
}

#' @rdname stab-fncs
#' @details combine_vs_tab: create a variable table by combining a variable
#'     table and a surv table
#' @export
combine_vs_tab <- function(vtab, stab, group.name = NULL){
    vtab2 <- stab2vtab(stab = stab, group.name = group.name)
    nm <- c("term", "label", "group")
    if(any(vtab2$term %in% vtab$term)){
        if(all(vtab2$term %in% vtab$term) ){
            vtab
        } else {
            s <- paste0("partially overlapping stab and vtab information\n",
                        " (no overlap or complete overlap is ok)")
            stop(s)
        }
    } else {
        rbind(vtab[, nm], vtab2[, nm])
    }
}


################################################################################
##        older things related to 'surv list'                                 ##
################################################################################

#' @rdname stab-fncs
#' @details slist2stab: older versions specified surv-pair variables via a list
#'     (surv-list), this function converts a surv-list to a surv table.
#' @export
slist2stab <- function(sl){
    r <- data.frame(label = names(sl),
                    time = unlist(lapply(sl, function(x) x[1])),
                    event = unlist(lapply(sl, function(x) x[2])))
    rownames(r) <- NULL
    r
}

################################################################################
##         SANITY CHECKS AND TESTS                                            ##
################################################################################

if(FALSE){

    nm <- c("t.foo", "ev.foo", "t.bar", "ev.bar", "t.APA", "ev.apa",
            "foo", "en_tid", "en_event")
    (st1 <- extract_stab_from_names(nm))
    st2 <- rbind(st1, data.frame(label = c("monkey", "banan"),
                                 time = c("t.APA", "en_tid"),
                                 event = c("ev.apa", "ananas")))
    check_stab(st2)
    verify_stab(st2, nm)

    sl <- list(foo = c("t.foo", "ev.foo"), monkey = c("t.APA", "ev.apa"))
    slist2stab(sl)

        vt <- data.frame(term = c("sex", "age"),
                     label = c("Sex", "Age"),
                     group = "Demographics")
    st <- data.frame(label = c("Foo", "Bar"),
                     time = c("t.foo", "t.bar"),
                     event = c("ev.foo", "ev.bar"))
    stab2vtab(st)
    combine_vs_tab(vt, st)

}
