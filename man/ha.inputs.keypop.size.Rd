% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract-ha.R
\name{ha.inputs.keypop.size}
\alias{ha.inputs.keypop.size}
\title{Get input key population sizes}
\usage{
ha.inputs.keypop.size(
  ha.raw,
  direction = "wide",
  first.year = NULL,
  final.year = NULL
)
}
\arguments{
\item{ha.raw}{Goals ASM module data in raw format, as returned by
\code{read.raw.ha()}}

\item{direction}{Request "wide" (default) or "long" format data.}

\item{first.year}{First year of the projection. If \code{first.year=NULL}, it
will be filled in using \code{ha.inputs.first.year()}}

\item{final.year}{Final year of the projection. If \code{final.year=NULL}, it
will be filled in using \code{ha.inputs.final.year()}}
}
\value{
A data frame.
}
\description{
Extract input key population sizes. These are expressed as percentages of the
age-specific population (15-24 or 25+) by sex who are key population members.
For example, if the 15-24 MSM population size is 1.1, then 1.1% fo 15-24
males are assumed to be MSM.
}
