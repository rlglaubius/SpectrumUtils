% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract-dp.R
\name{dp.inputs.epp.epidemic.type}
\alias{dp.inputs.epp.epidemic.type}
\title{Get the epidemic type specified in EPP}
\usage{
dp.inputs.epp.epidemic.type(dp.raw, direction = "wide")
}
\arguments{
\item{dp.raw}{DemProj module data in raw format, as returned by
\code{read.raw.dp()}}

\item{direction}{Ignored; included for compatibility with similar functions.}
}
\value{
"GENERALIZED" or "CONCENTRATED"
}
\description{
Get the epidemic type specified in EPP
}
