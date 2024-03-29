% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract-dp.R
\name{dp.inputs.anc.testing}
\alias{dp.inputs.anc.testing}
\title{Get data on HIV testing during antenatal care}
\usage{
dp.inputs.anc.testing(
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
a data frame.
}
\description{
Extract Spectrum inputs on antenatal clinic (ANC) attendance and HIV testing
}
\section{Details}{


Spectrum users may enter validation data on HIV testing at ANC in their
Spectrum files. These consist of:

\enumerate{
\item{Program reported births: Live births reported to the national program}
\item{First ANC visits: number of women with a first ANC visit during their current pregnancy}
\item{Tested: number of women who received at least one HIV test at ANC}
\item{Tested HIV+: number of women who tested HIV+ at the first test of their current pregnancy}
\item{Known HIV+: number of women whose HIV-positive status was known at their first ANC visit}
\item{Known HIV-: number of women whose HIV-negative status was known at their first ANC visit}
\item{ANC HIV\%: HIV prevalence at ANC. Calculated as (Tested HIV+ + Known HIV+) / (Tested + Known HIV+)}
\item{Retested: number of women who were tested for HIV at least once after their first HIV test during their current pregnancy}
\item{Retested HIV+: number of women who tested HIV+ during retesting}
}

Some of these data were not collected in earlier versions of Spectrum and are
not available in projections last saved in those versions.
}

