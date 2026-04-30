## ----"setup", cache = FALSE, echo = FALSE, include = FALSE--------------------
library(knitr)
library(data.table)
library(survival)
library(survivalist)
library(lattice)
library(latticeExtra)
## devtools::load_all() ## when testing
opts_chunk$set(include = TRUE,
               echo = TRUE,
               cache = FALSE,
               fig.pos = 'htb')
opts_knit$set(eval.after = c('fig.cap', 'fig.scap'))
if(FALSE){
    setwd('vignettes')
    knitr::knit2pdf("survstuff.rnw", clean = TRUE)
    shell.exec("survstuff.pdf")
}

## ----"test-data"--------------------------------------------------------------
n <- 1000
rnd <- function(set) sample(set, size = n, replace = TRUE)
set.seed(20260424)
d <- data.table(
    id = 1:n,
    age = runif(n, 40, 80),
    sex = rnd(c("M", "F")),
    group = factor(rnd(LETTERS[1:2]))
)
d[, `:=`(foo = rexp(n, 1/(1900 + 500 * as.integer(sex == "M") -
                          300 * as.integer(group) +
                          2* age)),
         bar = rexp(n, 1/(2300 + 700 * as.integer(sex == "M") -
                          250 * as.integer(group) +
                          2.5 * age)),
         cens = runif(n, min = 180, max = 810)
)]
d[, `:=`(ev.Foo = fifelse(foo <= cens, 1L, 0L),
         t.Foo = round(fifelse(foo <= cens, foo, cens)),
         ev.Bar = fifelse(bar <= cens, 1L, 0L),
         t.Bar = round(fifelse(bar <= cens, bar, cens)))]
d[, c("foo", "bar", "cens") := NULL]
str(d)

## ----"stab"-------------------------------------------------------------------
extract_stab_from_names(names(d))

## ----"survfitted"-------------------------------------------------------------
survfitted(Surv(t.Foo, ev.Foo) ~ group, data = d) |> str()

## ----"survfit_galore"---------------------------------------------------------
gt <- d[, .("A / B" = group %in% LETTERS[1:2],
            "B / C" = group %in% LETTERS[2:3])]
sg <- survfit_galore( ~ sex, data = d, gtab = gt)

## ----"sg-plot", fig.cap = "An easy plot to make!"-----------------------------
xyplot(estimate ~ time | group + outcome, data = sg, group = sex,
       type = c("s", "g"), lwd = 2, xlim = c(-10,740),
       auto.key = list(space = "top"),
       xlab = "Days since index",
       ylab = "Kaplain-Meier estimate") |>
    latticeExtra::useOuterStrips()

## ----"ref-plot", fig.height = 4, fig.cap = "Reference plot for Foo and Bar."----
xyplot(1-estimate ~ time, data = survfit_galore( ~ 1, data = d),
       group = outcome, auto.key = list(space = "top"), type = c("s"), lwd = 2,
       xlab = "Days since index", ylab = "Inverted Kaplan-Meier estimate",
       xlim = c(-10, 740), ylim = c(-0.05,1.05))

## ----"rescale-1"--------------------------------------------------------------
D <- rescale_surv(data = d, FUN = \(x) x/365.25, strip = FALSE)
str(D)

## ----"rescale-plot", echo = FALSE, fig.height = 4, fig.cap = cap--------------
cap <- paste0("Changing the time scale.")
xyplot(1-estimate ~ time, data = survfit_galore( ~ 1, data = D),
       group = outcome, auto.key = list(space = "top"), type = c("s"), lwd = 2,
       xlab = "Years since index", ylab = "Inverted Kaplan-Meier estimate",
       xlim = c(-0.05, 2.05))

## ----"combine-1"--------------------------------------------------------------
cs <- combine_surv(surv = c("Foo", "Bar"), data = D, strip = FALSE)
str(cs)

## ----"cs-sanity-check", eval = FALSE, echo = FALSE----------------------------
# cs[, table(ev.Foo, ev.Bar, ev.Combined, useNA = "if")]

## ----"combine-1-plot", echo = FALSE, fig.height = 4, fig.cap = cap------------
cap <- paste0("Combining time-to-event data into a single variable.")
xyplot(1-estimate ~ time, data = survfit_galore( ~ 1, data = cs),
       group = outcome, auto.key = TRUE, type = c("s"), lwd = 2,
       xlab = "Years since index", ylab = "Inverted Kaplan-Meier estimate",
       xlim = c(-0.05, 2.05))

## ----"trunc-landmark"---------------------------------------------------------
ts <- truncate_surv(data = D, trunc = 1, strip = FALSE)
ls <- landmark_surv(data = D, landmark = 1,
                    strip = FALSE, reset = FALSE)
R <- rbind(ts[, type := "truncation"], ls[, type := "landmark"])
r <- survfit_galore( ~ type, data = R, t0 = FALSE)

## ----"trunc-landmark-plot", echo = FALSE, fig.height = 4, fig.cap = cap-------
cap <- paste0("Effect of truncating and landmarking.")
xyplot(1-estimate ~ time | outcome, data = r, groups = type,
       auto.key = list(space = "top"), type = c("s"), lwd = 2,
       xlab = "Years since index", ylab = "Kaplan-Meier estimate",
       xlim = c(-0.05, 2.05),
       panel = function(...){
           panel.abline(v = 1, lty = 2)
           panel.xyplot(...)
       })

## ----"Surv"-------------------------------------------------------------------
str(Survclass_surv(data = D, strip = FALSE))

## ----"obs-data", echo = FALSE, include = TRUE, results = 'asis'---------------
n <- 1000
logistic <- function(z) 1 / (1 + exp(-z))

set.seed(20260430)
OD <- data.table(
    id = 1:n,
    male = factor(rbinom(n, 1, 0.5), levels = 0:1,
                  labels = c("Female", "Male")),
    age = runif(n, 40, 80)
)
OD[, old := as.integer(age > median(age))]
OD[, pe := logistic(2 - log(1.5)*(male == "Male") - .04 * age)]
OD[, E := factor(rbinom(n, 1, pe), levels = 0:1,
                 labels = c("ctrl", "case"))]
## OD[, table(E)]
## OD[, table(E, male)]
## OD[, table(E, old)]

OD[, pu := logistic(-4 + 0.02 * age +
                    log(1.5) * (male == "Male") +
                    log(1.3) * (E == "case"))]
OD[, Y := rexp(n, rate = pu)]
OD[, cens := runif(n, 1, 10)]
OD[, ev.U := as.integer(Y <= cens)]
OD[, t.U := fifelse(ev.U == 1, Y, cens)]

## sf0 <- survfitted(Surv(t.U, ev.U) ~ E, data = OD)
## xyplot(1-estimate ~ time, data = sf0, groups = E, type = "s")
## sf <- survfitted(Surv(t.U, ev.U) ~ E + male + old, data = OD)
## xyplot(1-estimate ~ time | male + old, data = sf, groups = E,
##        type = "s")
## coxph(Surv(t.U, ev.U) ~ E, data = OD)
## coxph(Surv(t.U, ev.U) ~ E + male, data = OD)
## coxph(Surv(t.U, ev.U) ~ E + age, data = OD)
## coxph(Surv(t.U, ev.U) ~ E + male + age, data = OD)

OD[, c("pe", "pu", "Y", "cens") := NULL]

g <- dable::dguide(OD, id = "id")
b <- dable::baseline(OD, guide = g, gtab = "E")
dable::blatex(b, caption = "Toy obesrvational data", label = "tab:OD")


## ----"cr_bias--sanity-check", echo = FALSE, eval = FALSE----------------------
# 
# coxph(Surv(t.U, ev.U) ~ E + male + age, data = OD)
# coxreg_bias(data = OD,
#             surv = "U",
#             main = "E",
#             bnry = "male")
# coxreg_bias(data = OD,
#             surv = "U",
#             main = "E",
#             bnry.manual = list("male2" = c(1.480123, 0.5336538, 0.4547872)))
# 

## ----"coef-sens"--------------------------------------------------------------
coxreg_change(
    data = OD,
    surv = data.frame(label="U",time="t.U",event="ev.U"),
                              ## 'stab' or common name of surv-pairs
    main = "E",               ## exposure term
    terms = c("male", "age"), ## adjustment variables
    uni = TRUE,               ## univariate effects?
    full = TRUE,              ## fully adjusted effects?
    inc = TRUE,               ## sequential inclusion?
    exc = TRUE                ## sequential exclusion?
)


## ----"cr-bias-1"--------------------------------------------------------------
coxreg_bias(
    data = OD,
    surv = "U",
    main = "E",    ## exposure term
    bnry = "male", ## binary adjustment variables
    real = "age"   ## real (continuous) adjustment variables
)[, c(1:2, 4:6, 9:11)]

## ----"cr-bias-2"--------------------------------------------------------------
cb <- coxreg_bias(data = OD,
                  surv = "U",
                  main = "E",
                  bnry = "male",
                  real = "age",
                  bnry.manual = list("confB" = c(1.3, .55, .45)),
                  real.manual = list("confR" = c(1.01, 50,  50)))
cb[, c(1, 3:5, 9:11)]
r <- attr(cb, "tidy")

## ----"cr-bias-plot", echo  =FALSE,  fig.cap = cap-----------------------------
stripplot(alt ~ HR | eff, data = r, pch = 16, cex = 1.2,
          x0 = r$ci1, x1 = r$ci2,
          xlim = c(min(r$ci1) - 0.05, max(r$ci2) + 0.05),
          panel = function(x, y, x0, x1, ...){
              panel.grid(h = -1, v = -1)
              panel.abline(v = 1, lty = 2)
              panel.segments(x0 = x0, x1 = x1, y0 = y, y1 = y)
              panel.stripplot(x, y, ...)
          },
          xlab = "Adjusted hazard ratio for main effect")

cap <- paste0("Adjusted hazard ratio for main effect when adjusting ",
              "for age and sex, as well as a confounder U ",
              "specified in the figure.")

