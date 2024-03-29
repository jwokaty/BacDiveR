% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getData.R
\name{getData}
\alias{getData}
\title{Get BacDive Data}
\usage{
getData(
  access_object,
  max_ids_to_retrieve = NULL,
  output_directory = getwd(),
  template_path = "inst/extdata/template.csv",
  update = FALSE,
  verbose = FALSE
)
}
\arguments{
\item{access_object}{BacDive Access Object BacDive access object}

\item{max_ids_to_retrieve}{Number of ids to retrieve. If no number specified,
it will assume the number of ids in the download.}

\item{output_directory}{character Path to the output csv. Defaults to current
directory.}

\item{template_path}{character Path to the record template.
Defaults to inst/extdata/template.csv.}

\item{update}{bool Downloads the file again if TRUE. Defaults to FALSE.}

\item{verbose}{character if TRUE write messages to console. Defaults to
FALSE.}
}
\value{
list of lists of BacDive Data
}
\description{
Get BacDive Data
}
\examples{
access_object <- authenticate('username', 'password')
getData(access_object)
}
