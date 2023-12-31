% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/state2event.R
\name{state2event}
\alias{state2event}
\title{create indicators for state change}
\usage{
state2event(s)
}
\arguments{
\item{s}{a factor of states}
}
\value{
a data frame with indicators (named as the levels of s) for state
changes
}
\description{
For each state in 's', create indicator variables for the state
changes. This might be useful e.g. when setting up data for a multistate
model.
}
\examples{
d <- data.frame(id=1, tstart = 0:7, tstop = 1:8,
                state = factor(LETTERS[c(1,1,2,1,1,3,2,2)],
                               levels = LETTERS[4:1]))
cbind(d, state2event(d$state))
}
