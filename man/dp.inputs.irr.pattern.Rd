% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract-dp.R
\name{dp.inputs.irr.pattern}
\alias{dp.inputs.irr.pattern}
\title{Check which epidemic pattern is used to specify incidence rate ratios}
\usage{
dp.inputs.irr.pattern(dp.raw, direction = "wide")
}
\arguments{
\item{dp.raw}{DemProj module data in raw format, as returned by
\code{read.raw.dp()}}

\item{direction}{Ignored; included for compatibility with similar functions.}
}
\value{
the epidemic pattern name as a factor (see "Details" below for factor levels)
}
\description{
Check which epidemic pattern is used to specify incidence rate ratios
}
\section{Details}{


  Spectrum supports several ways of specifying epidemic patterns for
  determining incidence patterns by sex and age:
  \enumerate{
  \item{Generalized - default pattern for generalized epidemics}
  \item{Concentrated non-IDU - default pattern for concentrated epidemics driven by transmission modes other than injection drug use}
  \item{Concentrated IDU - default pattern for concentrated epidemics driven by injection drug use}
  \item{Fitted: fixed over time - deprecated, replaced by "Fitted to HIV prevalence or ART"}
  \item{Fitted: time-varying - deprecated, replaced by "Fitted to HIV prevalence or ART"}
  \item{Fitted to HIV prevalence or ART - pattern estimated from survey data}
  \item{CSAVR - pattern estimated while fitting CSAVR}
  }
}

