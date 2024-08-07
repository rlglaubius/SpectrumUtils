% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract-ha.R
\name{ha.inputs.configuration}
\alias{ha.inputs.configuration}
\title{Extract inputs listed in the Goals ASM "Configure" form}
\usage{
ha.inputs.configuration(ha.raw, direction = "wide")
}
\arguments{
\item{ha.raw}{Goals ASM module data in raw format, as returned by
\code{read.raw.ha()}}

\item{direction}{Ignored; included for compatibility with similar functions.}
}
\value{
a data frame
}
\description{
Extract secondary school gross enrollment rates, secondary pupil-teacher
ratios, frequency of teacher re-training, numbers of sex acts per partner,
and condom wastage parameters.
}
