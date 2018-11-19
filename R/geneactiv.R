#' GENEActiv Sample data
#'
#' @description GENEActive accelerometer data, collected from two
#'    Dutch persons living in the Amsterdam area, in a one-week period.
#'
#' Data were collected between 2018-06-01 and 2018-06-08, at 30Hz,
#' with a GENEactive wrist-worn accelerometer. Data were down-sampled
#' to 3Hz.
#'
#' @format A data frame with 14.753 observations on 4 variables:
#' \describe{
#'   \item{\code{id}}{Person id (integer)}
#'   \item{\code{timestamp}}{Datetime of measurement (POSIXct)}
#'   \item{\code{x}}{Acceleration on x-axis}
#'   \item{\code{y}}{Acceleration on y-axis}
#'   \item{\code{z}}{Acceleration on z-axis}
#'   \item{\code{light}}{Light sensor}
#'   \item{\code{temperature}}{Temperature in degrees Celcius}
#' }
#'
#' @source https://www.activinsights.com/actigraphy/geneactiv-original/
#'
#' @docType data
#' @keywords datasets
#'
#' @examples
#' # number of samples
#' nrow(geneactiv)
#'
#' # number of assessments, per person
#' table(geneactiv$id)
#'
#' # plot one hour of raw data, of person 1
#' library(ggplot2)
#' library(tidyr)
#' d <-  subset(geneactiv,
#'              timestamp > "2018-06-01 13:00" &
#'              timestamp < "2018-06-01 14:00" &
#'              id == 1)
#'
#' d <- gather(d,
#'             key = "sensor", value = "value",
#'             x, y, z, temperature, light)
#'
#' ggplot(d, aes(timestamp, value)) +
#'   geom_point(alpha = .3,  shape = 16, size = .5) +
#'   facet_grid(rows = vars(sensor) , scales = "free_y")
"geneactiv"

