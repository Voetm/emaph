#' Data from a CASPAR pilot study.
#'
#' @description CASPAR pilot 2017
#'
#' @format A data frame with 5598 observations on the following 6 variables:
#' \describe{
#'
#'   \item{\code{id}}{Participant id}
#'   \item{\code{timestamp}}{Date/time of measurement}
#'   \item{\code{form}}{item set}
#'   \item{\code{round}}{assessment round}
#'   \item{\code{item}}{item}
#'   \item{\code{score}}{item score}
#' }
#'
#' @details Data retrieved from healthy subjects.
#'
#' @source Wouter van Ballegooijen / Chani Nuij
#'
#' @docType data
#' @keywords datasets
#'
#' @examples
#' # number of assessments
#' nrow(caspar)
#'
#' library(tidyverse)
#' d <- caspar %>% filter(id == 11)
#' ggplot(d, aes(x = timestamp, y = score)) +
#'   geom_point() + geom_line() +
#'   facet_wrap(~ item)
#'
#' library(zoo)
#' e <- d %>% group_by(scale = substring(d$item, 1, 3)) %>%
#'   mutate( rm_score = rollapply(score, 6, FUN = mean,
#'                                partial = TRUE, align = "right"))
#' ggplot(e, aes(x = timestamp, y = rm_score)) +
#'   geom_point(aes(y = score), alpha = .1) + geom_line() +
#'   facet_wrap(~ scale, ncol = 5)
#'
#'
"caspar"

