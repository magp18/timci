% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{export_df2csv}
\alias{export_df2csv}
\title{Write dataframe to a CSV file format, with a filename consisting of a prefix and a date stamp}
\usage{
export_df2csv(df, dirname, prefix)
}
\arguments{
\item{df}{dataframe.}

\item{dirname}{directory where the CSV file will be created.}

\item{prefix}{filename prefix}
}
\value{
This function returns the creation timestamp of the CSV file
}
\description{
Write dataframe to a CSV file format, with a filename consisting of a prefix and a date stamp
}
