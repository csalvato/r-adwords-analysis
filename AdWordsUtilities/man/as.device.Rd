% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as.device.r
\name{as.device}
\alias{as.device}
\title{As Device}
\usage{
as.device(device_vector)
}
\arguments{
\item{device_vectory}{A vector of device values from AdWords.}
}
\description{
This function converts the values in AdWords vectors to dt for desktop or mb for mobile
}
\examples{
as.device(device_vector)
as.device(google_adwords_report$device)
}

