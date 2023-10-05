##' @title fix censored dates
##' @description Fix censored dates of the type '20110000' or '20110200' by
##'     selecting the midpoint of the censored time interval. If a lower bound
##'     is given, the midpoint between this point and the end of the censored
##'     interval is chosen.
##' @param x character dates (possibly censored) in format '%Y%m%d'
##'     e.g. "20070101" (if valid), or "20070100" (if censored)
##' @param low.bound Date, the lower bound. A vector of the same length as 'x'
##'     (or of length 1)
##' @export
impute00date <- function(x, low.bound = as.Date(NA_character_)){
    properties(x, class = "character", na.ok = FALSE)
    n <- length(x)
    properties(low.bound, class = "Date", length = c(1, n), na.ok = TRUE)
    dt <- data.table(x = x, low.bound = low.bound)[
      , x00 := as.integer(grepl("00$", x))][
      , xdate := fifelse(x00 == 0,
                         yes = as.Date(x, format = "%Y%m%d"),
                         no = as.Date(NA_character_))][
      , bad.format := as.integer(x00 == 0 & is.na(xdate))][
      , bad.bound := as.integer(x00 == 0 &
                                bad.format == 0 &
                                low.bound > xdate)][
      , x00date := as.Date(NA_character_)][
      , error.log := 0L]
    if(dt[, any(x00 == 1L)]){
        for(i in dt[, which(x00 == 1L)]){
            dt[i, c("x00date", "error.log") := {
                r <- tryCatch(expr = impute_a_00_date(x, low.bound),
                              error = function(e){
                                  ## warning(e)
                                  -1L
                              })
                .(if(r == -1) impute_a_00_date(x) else r,
                  if(r == -1) 1L else 0L)
            }]
        }
    }
    r <- dt[, fifelse(x00 == 0, xdate, x00date)]
    bif <- dt[, which(bad.format == 1)]
    if(length(bif) > 0 ){
        attr(r, "bad_input_format") <- bif
        s <- paste0("bad input format at indexes: ",
                    paste0(bif, collapse = ", "),
                    ". Indexes stored in output attribute 'bad_input_format'.")
        warning(s)
    }
    ibb <- dt[, which(bad.bound == 1 | error.log == 1)]
    if(length(ibb) > 0){
        attr(r, "input_below_bound") <- ibb
        s <- paste0("x below lower bound at indexes: ",
                    paste0(ibb, collapse = ", "),
                    ". Indexes stored in output attribute 'input_below_bound'.")
        warning(s)
    }
    r
}

if(FALSE){

    x <- c("20200101", "20200202",
           "20200300", "20200400",
           "20200000", "20200000",
           "20210230", "20200202",
           "20200300")
    low.bound <- as.Date(c("2019-12-01", "2020-02-10",
                           "2020-03-20", "2020-05-01",
                           "2020-03-01", "2021-02-01",
                           NA_character_, NA_character_,
                           NA_character_))

    r <- impute00date(x, low.bound)

}

##' @describeIn impute00date impute_a_00_date: the workhorse for impute00date
##'     (can only work with inputs of length 1)
##' @export
impute_a_00_date <- function(x, low.bound = as.Date(NA_character_)){
    properties(x, class = "character", length = 1, na.ok = FALSE)
    properties(low.bound, class = "Date", length = 1, na.ok = TRUE)
    if(nchar(x) != 8) stop("x does not have 8 characters")
    if(!grepl("00$", x)) stop("x does not end in '00'")
    y <- as.numeric(substr(x, 1, 4))
    m <- as.numeric(substr(x, 5, 6))
    m_miss <- m == 0
    x_lo <- as.Date(paste0(y, "-",
                            if(m_miss) "01" else prefixNchar1With0(m), "-",
                            "01"), format = "%Y-%m-%d")
    x_hi <- as.Date(paste0(y, "-",
                           if(m_miss) "12" else prefixNchar1With0(m), "-",
                           if(m_miss) "31" else days_in_month(y, m)),
                    format = "%Y-%m-%d")
    if(!is.na(low.bound) && low.bound > x_hi){
        s <- paste0("input '", x, "' is below lower bound ", low.bound)
        stop(s)
    }
    ignore_bound <- is.na(low.bound) || low.bound < x_lo
    if(ignore_bound){
        if(m_miss){
            date_in_what_remains_of_the_year(y, 1, 1)
        } else{
            date_in_what_remains_of_the_month(y, m, 1)
        }
    } else {
        bound_y <- as.numeric(substr(low.bound, 1, 4))
        bound_m <- as.numeric(substr(low.bound, 6, 7))
        bound_d <- as.numeric(substr(low.bound, 9, 10))
        if(m_miss){
            date_in_what_remains_of_the_year(
                y = bound_y,
                m = bound_m,
                d = bound_d
            )
        } else{
            date_in_what_remains_of_the_month(
                y = bound_y,
                m = bound_m,
                d = bound_d
            )
        }
    }
}

prefixNchar1With0 <- function(x){
    properties(x, length = 1, na.ok = FALSE)
    if(nchar(x)==1) paste0("0", x) else x
}

create_date <- function(y, m, d){
    properties(y, class = c("numeric", "integer"), length = 1, na.ok = FALSE)
    properties(m, class = c("numeric", "integer"), length = 1, na.ok = FALSE)
    properties(d, class = c("numeric", "integer"), length = 1, na.ok = FALSE)
    as.Date(paste(c(y, prefixNchar1With0(m), prefixNchar1With0(d)), collapse = "-"))
}

days_in_month <- function(y, m){
    properties(y, class = c("numeric", "integer"), length = 1, na.ok = FALSE)
    properties(m, class = c("numeric", "integer"), length = 1, na.ok = FALSE)
    if(!m %in% 1:12) stop("m needs to be integer; 1 <= m <= 12")
    if(!y %in% 1582:3000){
        y <- as.integer(y)
        if(y < 1582) warning("before start of gregorian calendar")
        if(y > 3000) warning("woha! thats far into the future")
    }
    ref <- as.integer(c(31,28,31,30,31,30,31,31,30,31,30,31))
    leap <- y %% 4 == 0 && (y %% 100 != 0 || y %% 400 == 0)
    ref[m] + if(m == 2 & leap) 1L else 0L
}

days_in_year <- function(y){
    properties(y, class = c("numeric", "integer"), length = 1, na.ok = FALSE)
    leap <- y %% 4 == 0 && (y %% 100 != 0 || y %% 400 == 0)
    if(leap) 366L else 365L
}

what_day_in_year <- function(y, m, d){
    properties(y, class = c("numeric", "integer"), length = 1, na.ok = FALSE)
    properties(m, class = c("numeric", "integer"), length = 1, na.ok = FALSE)
    properties(d, class = c("numeric", "integer"), length = 1, na.ok = FALSE)
    leap <- y %% 4 == 0 && (y %% 100 != 0 || y %% 400 == 0)
    ref <- if(leap){
               as.integer(c(31,29,31,30,31,30,31,31,30,31,30,31))
           } else {
               as.integer(c(31,28,31,30,31,30,31,31,30,31,30,31))
           }
    d + if(m > 1) sum(ref[1:(m-1)]) else 0
}

what_date_in_year <- function(y, n){
    properties(y, class = c("numeric", "integer"), length = 1, na.ok = FALSE)
    properties(n, class = c("numeric", "integer"), length = 1, na.ok = FALSE)
    if(n > days_in_year(y)) stop("n larger than number of days in year")
    if(n < 1) stop("n must be a positive integer")
    leap <- y %% 4 == 0 && (y %% 100 != 0 || y %% 400 == 0)
    ref <- if(leap){
               as.integer(c(31,29,31,30,31,30,31,31,30,31,30,31))
           } else {
               as.integer(c(31,28,31,30,31,30,31,31,30,31,30,31))
           }
    cr <- cumsum(ref)
    l <- n > cr
    m <- 1 + if(any(l)) max(which(l)) else 0
    create_date(y, m, n - if(m>1) cr[m-1] else 0)
}

date_in_what_remains_of_the_month <- function(y, m, d){
    properties(y, class = c("numeric", "integer"), length = 1, na.ok = FALSE)
    properties(m, class = c("numeric", "integer"), length = 1, na.ok = FALSE)
    properties(d, class = c("numeric", "integer"), length = 1, na.ok = FALSE)
    if(m == 0) stop("month given as 0")
    D <- days_in_month(y, m)
    if(d > D) stop("bad date specification")
    d_new <- floor((D + d) / 2)
    create_date(y, m, d_new)
}

date_in_what_remains_of_the_year <- function(y, m, d){
    properties(y, class = c("numeric", "integer"), length = 1, na.ok = FALSE)
    properties(m, class = c("numeric", "integer"), length = 1, na.ok = FALSE)
    properties(d, class = c("numeric", "integer"), length = 1, na.ok = FALSE)
    N <- days_in_year(y)
    n <- what_day_in_year(y, m, d)
    n_new <- floor( (n+N) / 2)
    what_date_in_year(y, n_new)
}
