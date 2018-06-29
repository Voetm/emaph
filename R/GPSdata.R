#' GPS data
#'
#' @description A Google timeline GPS dataset, containing data of two
#'     participants.
#'
#' @format A data frame with 39438 observations on 4 variables:
#' \describe{
#'   \item{\code{id}}{Participant identifier (integer)}
#'   \item{\code{TimeStamp}}{Datetime of measurement (POSIXct)}
#'   \item{\code{lat}}{GPS Latitude (numeric)}
#'   \item{\code{lon}}{GPS Longitude (numeric)}
#'   \item{\code{accuracy}}{Estimated GPS accuracy (in meters)}
#' }
#'
#' @source Jeroen Ruwaard, Lisa Kooistra
#'
#' @docType data
#' @keywords datasets
#'
#' @examples
#' # number of assessments
#' nrow(GPSdata)
#'
"GPSdata"

