% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Outliervec.R
\name{OutlierVec}
\alias{OutlierVec}
\title{Detection of outliers based on standard deviation}
\usage{
OutlierVec(vector, std = 2)
}
\arguments{
\item{vector}{The vector where we detect outliers.}

\item{std}{The number of standard deviations for outliers to be detected}
}
\value{
Returns S3 object containing the index of the outliers
}
\description{
Detects observations that are more than a given number of standard devations away from the mean
}
\examples{
vector <- 1:10
Outliers<-OutlierVec(vector, std=1)
Outliers
}
