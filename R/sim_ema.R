#' Simulate EMA data
#'
#' @param plan a sample plan, as generated by \code{'sample_plan'}
#' @param mm_par a list, defining the parameters of a mixed effects model; the
#'     list should contain elements "fixed", "random", "error" and "phi".
#' @param lim optional upper and lower limits of data (defaults to NULL, in
#'     which case data values are unbounded).
#' @return a \code{data.frame}, containing the simulated data:
#'      \item{id}{Participant id (numeric)}
#'      \item{observation}{observation counter (within participant; numeric)}
#'      \item{day}{day counter (numeric)}
#'      \item{beep}{beep counter, within day (numeric)}
#'      \item{t}{time of day (string, in "\%H:\%M" format)}
#'      \item{Y}{Simulated EMA value}
#'
#' @import ggplot2
#' @import simstudy
#'
#' @export
#'
#' @examples
#' sample_plan <- sample_plan(n_participants = 20, n_days = 7)
#'
#' mm_par_a <- list(
#'   fixed  = c(intercept = 0,
#'              time      = 0),
#'   random = c(intercept = 0.1,
#'              time      = 0.005),
#'   error  = 0.2,
#'   phi    = 0.5
#' )
#' a <- sim_ema(plan = sample_plan,
#'              mm_par = mm_par_a)
#'
#' mm_par_b <- mm_par_a
#' mm_par_b$fixed['time']   <- -0.5
#' mm_par_b$random['time']  <- 0.005
#'
#' b <- sim_ema(plan = sample_plan,
#'              mm_par = mm_par_b)
#'
#' b$id <- b$id + max(a$id)
#'
#' c <- rbind(a, b)
#' c$group <- c(rep(0, nrow(a)), rep(1, nrow(b)))
#'
#' # plot
#' library(ggplot2)
#' ggplot(data = c, aes(x = time, y = Y)) +
#'        geom_line(aes(group = id, colour = factor(group))) +
#'        geom_point(size = .8) +
#'        geom_smooth(aes(group = factor(group), colour = factor(group)),
#'                    method = "loess", se = FALSE, size = 2) +
#'        xlab("Time")
#'
#' # test
#' library(nlme)
#' fm <- lme(Y ~ 1 + time * group ,
#'          random = ~ 1 + time | id,
#'          data = c,
#'          correlation = corAR1())
#' summary(fm)
#'
#'
sim_ema <- function(plan = sample_plan(),
                    mm_par = list(
                      fixed  = c(
                        intercept = 0,
                        time      = 0,
                        time2     = 0
                      ),
                      random = c(
                        intercept = 0,
                        time      = 0,
                        time2     = 0
                      ),
                      error  = 0.02,
                      phi    = 0
                    ),
                    lim = NULL) {
  # data will be stored in d -------
  d <- plan
  n_participants <- length(unique(d$id))
  n_timepoints <- max(plan$observation)

  # check mm_par ------------------
  mm_default_par <- list(
    fixed  = c(
      intercept = 0,
      time      = 0,
      time2     = 0
    ),
    random = c(
      intercept = 0,
      time      = 0,
      time2     = 0
    ),
    error  = 0.02,
    phi    = 0
  )
  if (is.null(mm_par))
    mm_par <- mm_default_par
  if (is.null(mm_par$fixed))
    mm_par$fixed <- mm_default_par$fixed
  if (is.na(mm_par$fixed["intercept"]))
    mm_par$fixed["intercept"] <- mm_default_par$fixed["intercept"]
  if (is.na(mm_par$fixed["time"]))
    mm_par$fixed["time"] <- mm_default_par$fixed["time"]
  if (is.na(mm_par$fixed["time2"]))
    mm_par$fixed["time2"] <- mm_default_par$fixed["time2"]

  if (is.null(mm_par$random))
    mm_par$random <- mm_default_par$random
  if (is.null(mm_par$random["intercept"]))
    mm_par$random["intercept"] <- mm_default_par$random["intercept"]
  if (is.na(mm_par$random["time"]))
    mm_par$random["time"] <- mm_default_par$random["time"]
  if (is.na(mm_par$random["time2"]))
    mm_par$random["time2"] <- mm_default_par$random["time2"]

  if (is.null(mm_par$error))
    mm_par$error <- mm_default_par$error
  if (is.null(mm_par$phi))
    mm_par$phi <- mm_default_par$phi


  # define the outcome ----

  # fixed effects
  fixef1 <- defDataAdd(varname = "b.intercept",
                       dist = "nonrandom",
                       formula = mm_par$fixed['intercept'])

  fixef2 <- defDataAdd(varname = "b.time",
                       dist = "nonrandom",
                       formula = mm_par$fixed['time'])

  fixef3 <- defDataAdd(varname = "b.time2",
                       dist = "nonrandom",
                       formula = mm_par$fixed['time2'])

  # random effects
  ranef1 <- defDataAdd(
    varname = "b.intercept.random",
    dist = "normal",
    formula = 0,
    variance = mm_par$random['intercept']
  )

  ranef2 <- defDataAdd(
    varname = "b.time.random",
    dist = "normal",
    formula = 0,
    variance = mm_par$random['time']
  )

  ranef3 <- defDataAdd(
    varname = "b.time2.random",
    dist = "normal",
    formula = 0,
    variance = mm_par$random['time2']
  )


  # correlated errors
  mu <- rep(0, n_timepoints)                # mean zero
  sigma <- rep(sqrt(mm_par$error), n_timepoints)  # variance

  dtCor <-
    genCorData(
      n_participants,
      mu = mu,
      sigma = sigma,
      rho = mm_par$phi,
      corstr = "ar1"
    )

  # all parameters, one row per id
  dtCor <- addColumns(fixef1, dtCor)
  dtCor <- addColumns(fixef2, dtCor)
  dtCor <- addColumns(fixef3, dtCor)
  dtCor <- addColumns(ranef1, dtCor)
  dtCor <- addColumns(ranef2, dtCor)
  dtCor <- addColumns(ranef3, dtCor)


  # create longitudinal data set
  longData <-
    addPeriods(
      dtCor,
      nPeriods = n_timepoints,
      idvars = "id",
      timevars = paste0("V", 1:n_timepoints),
      timevarName = "error"
    )
  names(longData)[2] <- "time"


  # add relative time (number of days, in fractions, since start of day 1)
  d$time <- as.POSIXct(paste(Sys.Date() + (d$day - 1), d$t))
  d$time <- difftime(d$time,
                     as.POSIXct(paste(as.Date(d$time[1]), "00:00:00")),
                     units = "days")
  d$time <- as.numeric(d$time)
  longData$time <- d$time

  # generate outcome based on definition
  ydef <- defDataAdd(
    varname = "Y",
    dist = "normal",
    formula = "b.intercept + b.intercept.random +
    b.time * time + b.time.random * time +
    b.time2 * time^2 + b.time2.random * time^2 +
    error"
  )
  longData <- addColumns(ydef, longData)

  # copy outcome to resul data set
  d$Y <- longData$Y
  d$id <- as.numeric(d$id)

  if (!is.null(lim)) {
    d$Y[d$Y < min(lim)] <- min(lim)
    d$Y[d$Y > max(lim)] <- max(lim)
  }

  d
}
