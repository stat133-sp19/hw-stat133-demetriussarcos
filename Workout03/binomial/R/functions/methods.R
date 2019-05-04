#' @export

plot.bindis <- function(x) {
  barplot(x$probability,
          xlab = "Successes",
          ylab = "Probabiity",
          col = "#6D9DFF",
          names.arg = x$success)
}


#' @export

plot.bincum <- function(x) {
  plot(x$cumulative,
       xlab = "Successes",
       ylab = "Probability",
       main = "Binomial Cumulative Distribution",
       col = "#2C67FF",
       type = "o",
       panel.first = grid()
  )
}


#' @export

print.binvar <- function(x) {
  cat(paste('"Binomial Variable"'))
  cat("\n\nParameters")
  cat("\n-number of trials:", paste(x$trials))
  cat("\n-prob of success:", paste(x$prob))
}


#' @export

summary.binvar <- function(x) {
  output <- list(trials = x$trials,
                 prob = x$prob,
                 mean = aux_mean(prob = x$prob, trials = x$trials),
                 variance = aux_variance(prob = x$prob, trials = x$trials),
                 mode = aux_mode(prob = x$prob, trials = x$trials),
                 skewness = aux_skewness(prob = x$prob, trials = x$trials),
                 kurtosis = aux_kurtosis(prob = x$prob, trials = x$trials))
  class(output) <- "summary.binvar"
  output
}


#' @export

print.summary.binvar <- function(x) {
  cat(paste('"Summary Binomial"'))
  cat("\n\nParameters")
  cat("\n-number of trials:", paste(x$trials))
  cat("\n-prob of success :", paste(x$prob))
  cat("\n\nMeasures")
  cat("\n-mean    :", paste(x$mean))
  cat("\n-variance:", paste(x$variance))
  cat("\n-mode    :", paste(x$mode))
  cat("\n-skewness:", paste(x$skewness))
  cat("\n-kurtosis:", paste(x$kurtosis))
}
