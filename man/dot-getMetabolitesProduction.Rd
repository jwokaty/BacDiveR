% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{.getMetabolitesProduction}
\alias{.getMetabolitesProduction}
\title{Get metabolite production data}
\usage{
.getMetabolitesProduction(metabolites_entries, verbose = verbose)
}
\arguments{
\item{verbose}{logical print messages. Default to FALSE.}

\item{metabolite_entries}{list of metabolites entries}
}
\value{
vector of character
}
\description{
Data will be in the format: metabolite production
Data will be separated by semicolons.
}
\examples{
.getMetabolitesProduction(metabolites_entries, TRUE)
}
