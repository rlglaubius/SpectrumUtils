% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract-dp.R
\name{dp.status.projection.valid}
\alias{dp.status.projection.valid}
\title{Check whether the projection was valid when last saved}
\usage{
dp.status.projection.valid(dp.raw, direction = "wide")
}
\arguments{
\item{dp.raw}{DemProj module data in raw format, as returned by
\code{read.raw.dp()}}

\item{direction}{Ignored; included for compatibility with similar functions.}
}
\value{
TRUE if the projection was valid, FALSE otherwise
}
\description{
Check whether the projection was valid when last saved
}
