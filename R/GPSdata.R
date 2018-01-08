#' GPS data
#'
#' @description a Google timelie GPS dataset of a single participant.
#'
#' @format A data frame with 587840 observations on the following 6 variables:
#' \describe{
#'   \item{\code{TimeStamp}}{Datetime of measurement}
#'   \item{\code{lat}}{GPS Latitude}
#'   \item{\code{lon}}{GPS Longitude}
#'   \item{\code{accuracy}}{GPS estimated accuracy (in meters)}
#'   \item{\code{activity}}{Activity prediction}
#'   \item{\code{activity_confidence}}{Altitude (in meters)}
#'   \item{\code{velocity}}{Velocity (in km per hour)}
#'   \item{\code{altitude}}{Altitude (in meters)}
#' }
#'
#' @details Data retrieved from single participant.
#'
#' @source \url{jruwaard.nl}
#'
#' @docType data
#' @keywords datasets
#'
#' @examples
#' # number of assessments
#' nrow(GPSdata)
#'
"GPSdata"

