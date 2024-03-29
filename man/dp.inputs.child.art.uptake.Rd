% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract-dp.R
\name{dp.inputs.child.art.uptake}
\alias{dp.inputs.child.art.uptake}
\title{ART uptake in children by age and year}
\usage{
dp.inputs.child.art.uptake(
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
Get the annual probability of ART initiation by single age among children not on ART
}
