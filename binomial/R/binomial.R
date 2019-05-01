library(ggplot2)

# Private function to validity of probability.
check_prob <- function(prob) {
  if (prob < 0 | prob > 1) {
    stop("Probabilty needs to be between 0 and 1.")
  }
  if (length(prob) > 1) {
    stop("Probabilty needs to be constant.")
  }
  TRUE
}

# Private function to validity of trials.
check_trials <- function(trials) {
  if (trials < 0) {
    stop("Trials must be greater than or all equal to zero.")
  }
  if (length(trials) > 1) {
    stop("Trials needs to be constant.")
  }
  TRUE
}

# Private function to validity of number of successes.
check_success <- function(success, trials) {
  if (!is.numeric(success)) {
    stop("invalid success vector.")
  }
  if (any(is.na(success))) {
    stop("success cannot contain missing values")
  }
  if (any(success < 0) | any(success > trials)) {
    stop("invalid values in success.")
  }
  if (length(success) < 1) {
    stop("success is not of adequate length.")
  }
  TRUE
}

# Auxilary method to calculate mean of a binomial distribution.
aux_mean <- function(trials, prob) {
  trials*prob
}

# Auxilary method to calculate mean of a binomial distribution.
aux_variance <- function(trials, prob) {
  trials * prob * (1 - prob)
}

# Auxilary method to calculate mode of a binomial distribution.
aux_mode <- function(trials, prob) {
  as.integer((trials * prob) + prob)
}

# Auxilary method to calculate skew of a binomial distribution.
aux_skewness <- function(trials, prob) {
  (1 - 2 * prob)/ (aux_variance(trials, prob) ** 0.5)
}

# Auxilary method to calculate kurtosis of a binomial distribution.
aux_kurtosis <- function(trials, prob) {
  (1 - 6 * prob * (1 - prob)) / aux_variance(trials, prob)
}

#' @title Bin Choosing
#' @description Uses factorial method to provide n choose k
#' @param n  all choices
#' @param k  number to choose
#' @return n choose k
#' @export
bin_choose <- function(n, k) {
  if (k > n) {
    stop("k cannot be greater than n")
  }
  if (k < 0) {
    stop("k cannot be less than 0")
  }
  factorial(n) / (factorial(k) * factorial(n - k))
}

#' @title Bin Probability
#' @description calculates binomial probability
#' @param success number of successful trials
#' @param trials total number of trials
#' @param prob probablity
#' @return binomial probability
#' @export
bin_probability <- function(success, trials, prob) {
  check_success(success, trials)
  check_prob(prob)
  check_trials(trials)
  bin_choose(trials, success) * (prob ** success) * ((1 - prob) ** (trials - success))
}

#' @title Bin Distribution
#' @description forms a table with which each success is given a probability
#' @param trials total number of trials
#' @param prob probability
#' @return data.frame -> result
#' @export
bin_distribution <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  success <- c()
  prob1 <- c()
  for (i in 1:(trials + 1)) {
    success[i] = i - 1
    prob1[i] = bin_probability(i - 1, trials, prob)
  }
  result <- data.frame(success = success, probability = prob1)
  class(result) <- c("bindis", "data.frame")
  result
}

#' @export
plot.bindis <- function(df) {
ggplot(df, aes(x = success, y = probability)) + geom_bar(stat = "identity")
}

#' @title Bin Cumulative
#' @description forms a table with which each success is given a probability and cumulative is added
#' @param trials total number of trials
#' @param prob probability
#' @return data.frame -> df
#' @export
bin_cumulative <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  df <- bin_distribution(trials, prob)
  df["cumulative"] <- cumsum(df["probability"])
  class(df) <- c("bincum", "data.frame")
  df
}

#' @export
plot.bincum <- function(df) {
  ggplot(df, aes(x = success, y = cumulative)) + geom_line() + geom_point()
}

#' @title Bin Variable
#' @description forms a binvar type list with trials and probability
#' @param trials total number of trials
#' @param prob probability
#' @return result
#' @export
bin_variable <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  result <- list(trials = trials, probability = prob)
  class(result) <- "binvar"
  result
}

#' @export
print.binvar <- function(bv, ...) {
  cat('"Binomial Variable"\n\n')
  cat(sprintf(" - number of trials:"), bv$trials, "\n")
  cat(sprintf(" - prob of success :"), bv$probability, "\n")
  invisible(bv)
}

#' @export
summary.binvar <- function(bv, ...) {
  trials <- bv$trials
  prob <- bv$probability
  freqs <- data.frame(
    trials = trials,
    prob = prob,
    mean = aux_mean(trials, prob),
    variance = aux_variance(trials, prob),
    mode = aux_mode(trials, prob),
    skewness = aux_skewness(trials, prob),
    kurtosis = aux_kurtosis(trials, prob))
  class(freqs) <- "summary.binvar"
  freqs
}

#' @export
print.summary.binvar <- function(bv, ...) {
  cat('"Summary Binomial"\n\n')
  cat('Parameters', "\n")
  cat(sprintf(" - number of trials:"), bv$trials[1], "\n")
  cat(sprintf(" - prob of success :"), bv$prob[1], "\n", "\n")
  cat('Measures', "\n")
  cat(sprintf(" - mean    :"), bv$mean[1], "\n")
  cat(sprintf(" - variance:"), bv$variance[1], "\n")
  cat(sprintf(" - mode    :"), bv$mode[1], "\n")
  cat(sprintf(" - skewness:"), bv$skewness[1], "\n")
  cat(sprintf(" - kurtosis:"), bv$kurtosis[1], "\n")
  invisible(bv)
}

#' @title Bin Mean
#' @description calculates mean of a binomial distribution
#' @param trials total number of trials
#' @param prob probability
#' @return mean
#' @export
bin_mean <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  aux_mean(trials, prob)
}

#' @title Bin Variance
#' @description calculates variance of a binomial distribution
#' @param trials total number of trials
#' @param prob probability
#' @return variance
#' @export
bin_variance <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  aux_variance(trials, prob)
}

#' @title Bin Mode
#' @description calculates mode of a binomial distribution
#' @param trials total number of trials
#' @param prob probability
#' @return mode
#' @export
bin_mode <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  aux_mode(trials, prob)
}

#' @title Bin Skewness
#' @description calculates skewness of a binomial distribution
#' @param trials total number of trials
#' @param prob probability
#' @return skewness
#' @export
bin_skewness <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  aux_skewness(trials, prob)
}

#' @title Bin Kurtosis
#' @description calculates kurtosis of a binomial distribution
#' @param trials total number of trials
#' @param prob probability
#' @return kurtosis
#' @export
bin_kurtosis <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  aux_kurtosis(trials, prob)
}






