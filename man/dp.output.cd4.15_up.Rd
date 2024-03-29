% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract-dp.R
\name{dp.output.cd4.15_up}
\alias{dp.output.cd4.15_up}
\alias{dp.output.cd4.15_49}
\title{Get Spectrum's estimate of adult PLHIV by CD4 category}
\usage{
dp.output.cd4.15_up(
  dp.raw,
  direction = "wide",
  first.year = NULL,
  final.year = NULL
)

dp.output.cd4.15_49(
  dp.raw,
  direction = "wide",
  first.year = NULL,
  final.year = NULL
)
}
\arguments{
\item{dp.raw}{DemProj module data in raw format, as returned by
\code{read.raw.dp()}}

\item{direction}{Request "wide" (default) or "long" format data.}

\item{first.year}{First year of the projection. If \code{first.year=NULL}, it
will be filled in using \code{dp.inputs.first.year()}}

\item{final.year}{Final year of the projection. If \code{final.year=NULL}, it
will be filled in using \code{dp.inputs.final.year()}}
}
\value{
A data frame.
}
\description{
Get the estimated number of adults by CD4 category and ART status
}
\section{Functions}{
\itemize{
\item \code{dp.output.cd4.15_up()}: Adults age 15+

\item \code{dp.output.cd4.15_49()}: Adults age 15-49

}}
