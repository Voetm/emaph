#' Import Google Location Location history
#'
#' Import a Google Location History JSON file into R.
#'
#' @param file a ".json" file, containing a Google location history.
#' @return a \code{data.frame}, with variables:
#'   \itemize{
#'     \item{id}{Participant identifier}
#'     \item{timestamp}{Datetime of measurement (POSICxt)}
#'     \item{lat}{GPS Latitude}
#'     \item{lon}{GPS Longitude}
#'     \item{accuracy}{Accuracy of location (in meters)}
#'   }
#'
#' @importFrom jsonlite fromJSON
#' @importFrom readr read_file
#'
#' @seealso https://takeout.google.com/settings/takeout/custom/location_history
#' (to export your location history into JSON); \code{\link{locations}} for a
#' data set containing four-week location histories of two people.
#'
#' @export
#'
#' @examples
#' example_json <- system.file("extdata", "google_timeline_sample.json",
#'                            package = "emaph")
#' d <- get_google_location_data(example_json)
#' head(d)
#'
get_google_location_data <- function(file = NULL) {

  # parse json file
  jsondata <- readr::read_file(file)
  gdat <- jsonlite::fromJSON(jsondata)
  locs <- gdat$locations

  d <- data.frame(timeStamp = as.numeric(locs$timestampMs) / 1000)
  d$timeStamp <- as.POSIXct(d$timeStamp, origin = "1970-01-01")

  d$lat <- locs$latitudeE7 / 1e7   # E7 -> GPS
  d$lon <- locs$longitudeE7 / 1e7
  d$accuracy <- locs$accuracy

  d
}



