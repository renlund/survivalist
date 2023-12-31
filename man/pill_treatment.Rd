% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pill_treatment.R
\name{pill_treatment}
\alias{pill_treatment}
\alias{pill_switch}
\title{treatment periods defined by pill consumption}
\usage{
pill_treatment(
  data,
  id = "id",
  t = "t",
  state = "state",
  pills = "pills",
  usage = "usage",
  capacity = "capacity",
  null.state = "",
  simplify = TRUE
)

pill_switch(data, null.state = "", simplify = TRUE)
}
\arguments{
\item{data}{a data.frame containing 'id', 't', 'state' and, optionally,
'run'.}

\item{id}{character; name of id variable}

\item{t}{character; name of time variable}

\item{state}{character; name of treatment variable}

\item{pills}{character; name of variable keeping track of pills added to
(treatment specific) inventory}

\item{usage}{either name of variable (character) keeping track of pill
usage, or the numeric value for all pill usage. If given as a name that
does not exist in data, the value 1 will be assigned.}

\item{capacity}{either name of variable (character) keeping track of upper
bound for the size of the pill inventory, or the numeric value for all
pill inventory capacities. If given as a name that does not exist in
data, the value Inf will be assigned.}

\item{null.state}{value for 'state' when treatment ends}

\item{simplify}{logical; if TRUE keep only minimal information enough to
keep track of changes in treatment.}
}
\value{
a data.frame
}
\description{
Given a data frame ('data') with individuals (identified by 'id'), treatment
('state') initiated (or continued) at 't' by adding a number of pills
('pills') to an inventory of such, we calculate the times at which treatment
ends and reverts back to the 'null.state' (or changes), assuming that pills
are consumed by 'usage' each time unit. Note 1: if new pills (for the same
treatment/state) arrives before the previous pills are consumed, the new
pills will be added to the inventory of pills, i.e. they can be stockpiled.
Note 2: if a different treatement is initiated before the pills of the
previous treatment are consumed, the old inventory is nullified and a new
treatment period begins. Note 3: duplicated times (per individual) will be
ignored. Note 4: a treatment will be ongoing as long as there are pills,
i.e. if e.g. usage is 2 and the current inventory of pills is 1, the
treatment will be considered active one more unit of time. Note 5:
'capacity' kicks in after the daily pills have been consumed, i.e. if you
initiate a treatment with a 100 pills but only have capacity for 10, you
will consume 'usage' pills, and then have 10 left in the inventory.
}
\section{Functions}{
\itemize{
\item \code{pill_switch()}: The workhorse for pill_treatment; a rigid
function that requires a data.table with variables named 'id' (which
should be a constant), 't' (which the data.table should be ordered on),
'state', 'pills' and 'usage'. Probably don't use this directly, there
are no checks or warnings of any kind with this function

}}
\examples{
data <- data.frame(
    id = 1,
    t =     c(  0, 10, 30, 35, 40),
    state = c("A","B","B","C","C"),
    pills = c(  4, 10, 10, 20, 20),
    usage = c(  3,  1,  1,  2,  1)
)
## note that initial state A will be active until t = 2
## note that the 10 remaining pills from state C initiated at 35 (with a
##   usage of 2 until) ending at 40 (when usage changes to 1) are added
##   to the inventory so that state C can remain active until t = 70
pill_treatment(data)
}
