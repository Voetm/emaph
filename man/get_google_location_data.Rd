% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_google_location_data.R
\name{get_google_location_data}
\alias{get_google_location_data}
\title{Import Google Location Location history}
\usage{
get_google_location_data(file = NULL)
}
\arguments{
\item{file}{a ".json" file, containing a Google location history.}
}
\value{
a \code{data.frame}, with variables:
  \itemize{
    \item{id}{Participant identifier}
    \item{timestamp}{Datetime of measurement (POSICxt)}
    \item{lat}{GPS Latitude}
    \item{lon}{GPS Longitude}
    \item{accuracy}{Accuracy of location (in meters)}
  }
}
\description{
Import a Google Location History JSON file into R.
}
\examples{
example_json <- system.file("extdata", "google_timeline_sample.json",
                           package = "emaph")
d <- get_google_location_data(example_json)
head(d)

}
\seealso{
https://takeout.google.com/settings/takeout/custom/location_history
(to export your location history into JSON); \code{\link{locations}} for a
data set containing four-week location histories of two people.
}
