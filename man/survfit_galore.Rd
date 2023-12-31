% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/survfit_galore.R
\name{survfit_galore}
\alias{survfit_galore}
\title{'survfit' for several outcomes and groups}
\usage{
survfit_galore(formula, data, surv = NULL, glist = NULL, ...)
}
\arguments{
\item{formula}{formula, only RHS of which is used}

\item{data}{data}

\item{surv}{specification of surv components}

\item{glist}{a named list of logical indices}

\item{...}{other arguments for survfit}
}
\value{
a data frame
}
\description{
Iterated application of 'survfitted' to all outcomes specificed by 'surv'
plus an added possibility of adding (possibly overlapping) groups by
supplying a list of indexes in 'glist'.
}
