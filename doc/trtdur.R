## ----"setup", cache = FALSE, echo = FALSE, include = FALSE--------------------
library(knitr)
library(data.table)
library(survival)
library(survivalist)
## devtools::load_all() ## when testing
opts_chunk$set(include = TRUE,
               echo = TRUE,
               cache = FALSE)
opts_knit$set(eval.after = c('fig.cap', 'fig.scap'))
if(FALSE){
    setwd('vignettes')
    knitr::knit2pdf("trtdur.rnw", clean = TRUE)
    shell.exec("trtdur.pdf")
}

## ----"fix-1"------------------------------------------------------------------
d <- rowwiseDT(
    id=, t=, state=, run=,
    1,    1,    "A",    5,
    1,   10,    "B",   15,
    1,   20,    "C",   50,
    1,   21,    "C",    9
)
(ft <- fixed_treatment(data = d, null.state = "*"))

## ----"fix-1-tdc"--------------------------------------------------------------
b <- data.table(id=1, tstart=0, tstop=50)
T0 <- tmerge(b, b, id = id, tstart = tstart, tstop = tstop)
tmerge(T0, ft, id = id, trt = tdc(t, state, init = "*"))

## ----"fix-2"------------------------------------------------------------------
d <- rowwiseDT(
    id=, t=,  run=,
    1,    1,     5,
    1,   10,    99,
    1,   20,    10
)
onoff_treatment(data = d)

## ----"pill-1"-----------------------------------------------------------------
d <- rowwiseDT(
    id=, t=, state=, pills=, usage=, capacity=,
    1,    1,    "A",     10,      2,       Inf,
    1,   10,    "A",   1000,      1,         5,
    1,   20,    "A",     10,      1,       Inf,
    1,   25,    "A",     10,      1,       Inf,
    1,  100,    "A",     50,      1,       Inf,
    1,  110,    "B",     50,      1,       Inf
)
pill_treatment(data = d)

## ----"burden-1"---------------------------------------------------------------
d <- rowwiseDT(
    id=, t=,  pills=,
    1,    1,      16
)
pb <- pill_burden(data = d, usage = 2, capacity = Inf, window = 4,
                  burden = "treatment", dep.rate = 1)
pb[, .(id, t, store.begin, store.end, use, trt, stat, cumstat)]

## ----"burden-2"---------------------------------------------------------------
pill_burden(data = d, usage = 2, capacity = Inf, window = 4,
            burden = "pill", dep.rate = 1)[
  , .(id, t, store.begin, store.end, use, trt, stat, cumstat)]

## ----"burden-333"-------------------------------------------------------------
pill_burden(data = d, usage = 2, capacity = Inf, window = 4,
            burden = "pill", dep.rate = 1,
            breaks = c(-1,0,5,10),
            break.labels = c("no burden",
                             "1-5 pill burden",
                             "6-10 pill burden"),
            simplify = TRUE)

