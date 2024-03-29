% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract-dp.R
\name{dp.inputs.survey.hiv.prevalence}
\alias{dp.inputs.survey.hiv.prevalence}
\title{Get HIV prevalence data entered from household surveys}
\usage{
dp.inputs.survey.hiv.prevalence(dp.raw, direction = "wide")
}
\arguments{
\item{dp.raw}{DemProj module data in raw format, as returned by
\code{read.raw.dp()}}

\item{direction}{Request "wide" (default) or "long" format data.}
}
\value{
A data frame.
}
\description{
Get HIV prevalence data from household surveys that have been entered into
Spectrum for validation of its HIV prevalence estimates.
}
\section{Details}{


  Spectrum can store data from up to five household surveys. Data
  consist of HIV prevalence estimates by sex (male, female, both)
  and five-year age group (0-4, 5-9, ..., 75-79, 80+). Five statistics
  are stored for each group:

  \enumerate{
  \item{Number - not used}
  \item{Prevalence - HIV prevalence point estimate}
  \item{Lower - 95\% confidence interval lower bound}
  \item{Upper - 95\% confidence interval upper bound}
  \item{N - sample size. Countries may enter non-integral survey-weighted sample sizes}
  }
}

