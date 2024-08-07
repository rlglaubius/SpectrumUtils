% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract-ha.R
\name{ha.inputs.final.year}
\alias{ha.inputs.final.year}
\title{Get the final year of a Spectrum projection}
\usage{
ha.inputs.final.year(ha.raw, direction = "wide")
}
\arguments{
\item{ha.raw}{Goals ASM module data in raw format, as returned by
\code{read.raw.ha()}}

\item{direction}{Ignored; included for compatibility with similar functions.}
}
\value{
the final year of the projection.
}
\description{
Get the final year of a Spectrum projection
}
\section{Details}{


Analogous functions are available for other modules. For example, \code{dp.inputs.final.year} will
extract the final year of the projection via the DemProj module. This is
expected to match \code{ha.inputs.final.year}
}

