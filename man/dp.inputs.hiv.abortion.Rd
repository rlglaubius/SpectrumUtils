% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract-dp.R
\name{dp.inputs.hiv.abortion}
\alias{dp.inputs.hiv.abortion}
\title{Data on abortions among HIV-positive women}
\usage{
dp.inputs.hiv.abortion(
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
Get input data on the number of pregnancies among HIV-positive women
terminated by abortion
}
\section{Details}{


  Abortion data can be entered in Spectrum as numbers or percentages
  of HIV-positive women. These units can vary from year to year. When
  \code{direction="wide"}, the return value includes separate rows
  numbers and percentages. When \code{direction="long"}, the data frame
  will include one row per year with the unit indicated.
}

