#' Generate time series used in "Feature Extraction"-chapter of the manual.
#'
#' @param seed random seed.
#' @param n_measurements_per_day number of data points per day.
#' @param n_days assessment period (in days).
#' @param m mean at start (first element) and end (second element) of s.
#' @param sdev sd at start (first element) and end (second element) of s.
#' @param ampl amplitude of circadian rhythms (1-day, 7-day period) in s.
#' @param range min, max range of s.
#' @param missingness_prob minimal and maximal missingness probability of s.
#' @param autocorrelation strength of autocorrelation component in s.
#' @param plot if TRUE, the series is plotted (s ~ t). Default is FALSE.
#'
#' @return a data.frame, containing the generate timeseries
#'
#' @import ggplot2
#'
#' @export
#'
#' @examples
#' d <- generate_features_dataset(seed = 123, plot = TRUE)
#' head(d)
#'
#' fm <- lm(s ~ t, d)
#' summary(fm)

generate_features_dataset <- function(seed = NULL,
                                      n_measurements_per_day = 5,
                                      n_days = 21,
                                      m = c(3, 6),
                                      sdev = c(0.5, 0.5),
                                      ampl = c(1, 4),
                                      range = c(0, 10),
                                      missingness_prob = c(0.1, 0.5),
                                      autocorrelation = .6,
                                      plot = FALSE) {
  # set random seed, if defined
  if (!is.null(seed)) {
    set.seed(seed)
  }
  n <- n_measurements_per_day * (n_days)

  # time
  t <- seq(
    from = 0,
    to = (n - 1) * 1 / n_measurements_per_day,
    by =  1 / n_measurements_per_day
  )

  # signal (empty)
  s <- numeric(length(t))

  # periodicity
  s <-
    seq(ampl[1], ampl[2], length.out = n) * sin(1 * t * 2 * pi) # day
  s <-
    s + 0.5 * seq(ampl[1], ampl[2], length.out = n) * sin(1 / 7 * t * 2 * pi) # week

  # mean + trend + sd
  s <- s + stats::rnorm(
    n,
    mean = seq(m[1], m[2], length.out = n),
    sd =  seq(sdev[1], sdev[2], length.out = n)
  )

  # autocorrelation component
  amount = 1
  s <-
    s + amount * stats::arima.sim(model = list(ar = autocorrelation), n = n)

  # missingness
  missings <-
    stats::rbinom(
           n = n,
           size = 1,
           prob = seq(from = missingness_prob[1],
                      to = missingness_prob[2],
                      length.out = n))
  s[missings == 1] <- NA

  # in range
  s[s > range[2]] <- range[2]
  s[s < range[1]] <- range[1]

  # combine in data.frame
  d <- data.frame(t, s)

  if (plot == TRUE) {
    g <- ggplot(subset(d,!is.na(d$s)), aes(t, s)) +
      geom_point() +
      geom_line() +
      scale_x_continuous() +
      scale_y_continuous(limits = range)
    print(g)
  }

  # return
  d
}
