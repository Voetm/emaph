#' Generate a signal-contingent EMA sampling plan
#'
#' @param n_participants number of participants in the study
#' @param n_days study period
#' @param times times (or time-periods) at which assessments are requested; a
#'     character vector with time elements (e.g., "11:00", "23:00"), with as
#'     many elements as n_beeps_per_day. Times can be specified as time points,
#'     ("11:15"), or as periods ("11:00-12:00"), in which case a random time
#'     point is chosen, per participant, in the designated period.
#' @param plot if TRUE, the sample plan is plotted. Default is FALSE.
#'
#' @return a data.frame, containing the sample plan.
#'
#' @import ggplot2
#'
#' @export
#'
#' @examples
#' plan <- sample_plan(plot = TRUE)
#' head(plan)
#'
sample_plan <- function(
  n_participants = 7,
  n_days = 2,
  times = c("08:00-10:00", "12:30", "17:00-19:00"),
  plot = FALSE
) {

  n_beeps_per_day <- length(times)

  id <- day <- beep <- NULL # avoid CRAN NSE notes
  plan <- as.data.frame(
    expand.grid(
      id = 1:n_participants,
      day = 1:n_days,
      beep = 1:n_beeps_per_day)
  )

  plan$id <- factor(plan$id)

  plan <- plan[order(plan$id, plan$day, plan$beep), ]
  plan$observation <- rep(1:(n_days*n_beeps_per_day), n_participants)

  plan$t <- rep(times, n_participants * n_days)

  if (any(grep("-", times))) {
    t <- strsplit(plan$t, "-")
    t <- lapply(t, function(x) as.numeric(as.POSIXct(paste("1970-01-01", x))))
    tr <- unlist(lapply(t, function(x) {
     if (length(x) > 1) {
       return(sample(x[1]:x[2], 1))
     } else {
       return(x[1])
     }
    }))
    tr <- as.POSIXct(tr, origin = "1970-01-01")
    plan$t <- substring(tr, 12, 16)
  }

  plan <- plan[c("id", "observation", "day", "beep", "t")]
  rownames(plan) <- 1:nrow(plan)

  # plot
  if (plot == TRUE) {
    d <- plan
    d$t <- as.POSIXct(paste(Sys.Date() + (d$day - 1), d$t))
    g <- ggplot(d, aes(x = id, y = t)) +
      geom_point(size = 2) +
      geom_segment(aes(x = id,
                       xend = id,
                       y = min(t),
                       yend = max(t)),
                   linetype = "dashed",
                   size = 0.1) +
      coord_flip()

    print(g)
  }
  plan
}
