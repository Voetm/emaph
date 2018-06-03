#' Simulate EMA data
#'
#' @param n_participants number of participants in the study
#' @param n_days study period
#' @param times times (or time-periods) at which assessments are requested; a
#'     character vector with time elements (e.g., "11:00", "23:00"), with as
#'     many elements as n_beeps_per_day. Times can be specified as time points,
#'     ("11:15"), or as periods ("11:00-12:00"), in which case a random time
#'     point is chosen, per participant, in the designated period.
#'
#' @param plot if TRUE, the sample plan is plotted. Default is FALSE.
#' @param intercept intercept
#' @param intercept_var variance of intercept, between individuals
#' @param slope  slope
#' @param slope_var variance of slope, between individuals
#' @param slope2 slope, squared
#' @param slope2_var variance of slope squared
#' @param error_var error variance
#' @param rho autocorrelation of errors
#'
#' @return a data.frame, containing the simulated data.
#'
#' @import ggplot2
#' @import simstudy
#'
#' @export
#'
#' @examples
#' a <- sim_ema(n_participants = 20,
#'              slope = -0.05,
#'              slope_var = 0.0005)
#' b <- sim_ema(n_participants = 20,
#'              slope = -0.5,
#'              slope_var = 0.0005,
#'              slope2 = 0.02,
#'              slope2_var = 0)
#' b$id <- b$id + max(a$id)
#'
#' c <- rbind(a, b)
#' c$group <- c(rep(0, nrow(a)), rep(1, nrow(b)))
#'
#' library(ggplot2)
#' ggplot(data = c, aes(x = time, y = Y)) +
#'        geom_line(aes(group = id, colour = factor(group))) +
#'        geom_point(size = .8) +
#'        #geom_smooth(aes(group = factor(id), colour = factor(group)), method = "lm", se = FALSE) +
#'        #geom_smooth(aes(group = factor(group)), method = "lm", se = FALSE) +
#'        xlab("Time")
#'
#' # test
#' library(nlme)
#' fm <- lme(Y ~ 1 + (time + I(time^2)) * group , random = ~ 1 + time | id,
#'          data = c,
#'          correlation = corAR1())
#' summary(fm)
#'

sim_ema <- function(n_participants = 5,
                    n_days = 7,
                    times = c("08:00-10:00", "12:30", "17:00-19:00"),
                    plot = FALSE,
                    intercept        = 0,
                    intercept_var    = 0,
                    slope            = 0,
                    slope_var        = 0,
                    slope2           = 0,
                    slope2_var       = 0,
                    error_var        = 0.2,
                    rho = 0.5) {

  n_timepoints = length(times) * n_days


  d = sample_plan(
    n_participants = n_participants,
    n_days = n_days,
    times = times,
    plot = FALSE
  )

  #define the outcome
  ydef <- defDataAdd(
    varname = "Y",
    dist = "normal",
    formula = "intercept + randomIntercept +
               slope * time + randomSlope * time +
               slope2 * time^2 + randomSlope2 * time^2 +
               error"
  )

  fixef1 <- defDataAdd(varname = "intercept",
                       dist = "nonrandom",
                       formula = intercept)

  fixef2 <- defDataAdd(varname = "slope",
                       dist = "nonrandom",
                       formula = slope)

  fixef3 <- defDataAdd(varname = "slope2",
                       dist = "nonrandom",
                       formula = slope2)

  ranef1 <- defDataAdd(
    varname = "randomIntercept",
    dist = "normal",
    formula = 0,
    variance = intercept_var
  )

  ranef2 <- defDataAdd(
    varname = "randomSlope",
    dist = "normal",
    formula = 0,
    variance = slope_var
  )

  ranef3 <- defDataAdd(
    varname = "randomSlope2",
    dist = "normal",
    formula = 0,
    variance = slope2_var
  )


  # define the correlated errors
  mu <- rep(0, n_timepoints)
  sigma <- rep(error_var, n_timepoints)

  dtCor <-
    genCorData(
      n_participants,
      mu = mu,
      sigma = sigma,
      rho = rho,
      corstr = "ar1"
    )
  dtCor <- addColumns(fixef1, dtCor)
  dtCor <- addColumns(fixef2, dtCor)
  dtCor <- addColumns(fixef3, dtCor)
  dtCor <- addColumns(ranef1, dtCor)
  dtCor <- addColumns(ranef2, dtCor)
  dtCor <- addColumns(ranef3, dtCor)


  # create longitudinal data set and generate outcome based on definition
  longData <-
    addPeriods(
      dtCor,
      nPeriods = n_timepoints,
      idvars = "id",
      timevars = paste0("V", 1:n_timepoints),
      timevarName = "error"
    )
  names(longData)[2] <- "time"

  d$time <- as.POSIXct(paste(Sys.Date() + (d$day - 1), d$t))
  d$time <- difftime(d$time,
                     as.POSIXct(paste(as.Date(d$time[1]), "00:00:00")),
                     units = "days")
  d$time <- as.numeric(d$time)
  longData$time <- d$time

  longData <- addColumns(ydef, longData)

  d$Y <- longData$Y
  d$id <- as.numeric(d$id)
  d
}


