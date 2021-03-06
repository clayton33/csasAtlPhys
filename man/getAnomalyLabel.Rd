% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/labels.R
\name{getAnomalyLabel}
\alias{getAnomalyLabel}
\title{Provide axis names for anomaly labels}
\usage{
getAnomalyLabel(item, sep = "", bold = FALSE)
}
\arguments{
\item{item}{A character string the desired label. Supported labels include \code{temperatureAnomaly},
\code{salinityAnomaly}, and \code{sigmaThetaAnomaly}.}

\item{sep}{An optional character string inserted between the unit and the unit bracket that encloses it.
If not provided, the default \code{oceUnitSep} is used.}

\item{bold}{A logical value indicating whether or not to return a bold version of the label.}
}
\description{
This function provides the axis name for anomaly variables
}
\author{
Chantelle Layton
}
