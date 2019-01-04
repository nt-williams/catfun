
#' Tests for equality of proportions
#'
#' Run tests for equality of proportions using Wald normal approximation, Wilson approximation, or using an exact test.


prop_test <- function(x, n, p = NULL, method = c("wald", "wilson", "agresti-couli", "jeffreys",
                                                 "modified wilson", "wilsoncc", "modified jeffreys",
                                                 "clopper-pearson", "arcsine", "logit", "witting", "pratt"),
                      alternative = c("two.sided", "less", "greater"),
                      conf.level = 0.95, correct = FALSE) {

  method <- match.arg(method)
  alternative <- match.arg(alternative)

  if (!is.null(p)) {
    p <- p
  } else {
    p <- 0.5
  }

  exact <- binom.test(x, n)
  exact_p <- exact$p.value
  exact_ci <- exact$conf.int[1:2]

  if (correct == FALSE) {
    test <- prop.test(x = x, n = n, p = p, alternative = alternative, correct = FALSE)
  } else {
    test <- prop.test(x = x, n = n, p = p, alternative = alternative, correct = TRUE)
    method <- "wilsoncc"
  }

  statistic <- test$statistic
  df <- test$parameter
  p_value <- test$p.value
  estimate <- test$estimate

  if (method == "wald") {

    ci <- BinomCI(x, n, method = "wald", conf.level = conf.level)
    method_est <- ci[1, 1]
    method_ci <- ci[1, 2:3]

  } else if (method == "wilson") {

    ci <- BinomCI(x, n, method = "wilson", conf.level = conf.level)
    method_est <- ci[1, 1]
    method_ci <- ci[1, 2:3]

  } else if (method == "agresti-couli") {

    ci <- BinomCI(x, n, method = "agresti-couli", conf.level = conf.level)
    method_est <- ci[1, 1]
    method_ci <- ci[1, 2:3]

  } else if (method == "jeffreys") {

    ci <- BinomCI(x, n, method = "jeffreys", conf.level = conf.level)
    method_est <- ci[1, 1]
    method_ci <- ci[1, 2:3]

  } else if (method == "modified wilson") {

    ci <- BinomCI(x, n, method = "modified wilson", conf.level = conf.level)
    method_est <- ci[1, 1]
    method_ci <- ci[1, 2:3]

  } else if (method == "wilsoncc") {

    ci <- BinomCI(x, n, method = "wilsoncc", conf.level = conf.level)
    method_est <- ci[1, 1]
    method_ci <- ci[1, 2:3]

  } else if (method == "modified jeffreys") {

    ci <- BinomCI(x, n, method = "modified jeffreys", conf.level = conf.level)
    method_est <- ci[1, 1]
    method_ci <- ci[1, 2:3]

  } else if (method == "clopper-pearson") {

    ci <- BinomCI(x, n, method = "clopper-pearson", conf.level = conf.level)
    method_est <- ci[1, 1]
    method_ci <- ci[1, 2:3]

  } else if (method == "arcsine") {

    ci <- BinomCI(x, n, method = "arcsine", conf.level = conf.level)
    method_est <- ci[1, 1]
    method_ci <- ci[1, 2:3]

  } else if (method == "logit") {

    ci <- BinomCI(x, n, method = "logit", conf.level = conf.level)
    method_est <- ci[1, 1]
    method_ci <- ci[1, 2:3]

  } else if (method == "witting") {

    ci <- BinomCI(x, n, method = "witting", conf.level = conf.level)
    method_est <- ci[1, 1]
    method_ci <- ci[1, 2:3]

  } else if (method == "pratt") {

    ci <- BinomCI(x, n, method = "pratt", conf.level = conf.level)
    method_est <- ci[1, 1]
    method_ci <- ci[1, 2:3]

  }
}


