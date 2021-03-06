% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{separate_col}
\alias{separate_col}
\title{Separate a character column into multiple columns with a regular expression}
\usage{
separate_col(.data, col, into, sep = "_")
}
\arguments{
\item{.data}{A data frame.}

\item{col}{Column name to separate in.}

\item{into}{Names of new variables to create as character vector.}

\item{sep}{Separator between columns.}
}
\value{
A data frame with the separated columns
}
\description{
Separate a character column into multiple columns with a regular expression
}
\examples{
df <- data.frame(a = c("first_second", "my_vec"))
df
separate_col(df, a, into = c("v1", "v2"))
}
