% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{.toVector}
\alias{.toVector}
\title{Convert a string separated by commas into a vector of strings}
\usage{
.toVector(a_string)
}
\arguments{
\item{a_string}{character representing a comma-separated list}
}
\value{
list of character
}
\description{
Attempts to parse items separated by double quotes
}
\examples{
toVector('“Isolation, sample, environment”, isolation')
}
