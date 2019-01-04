
#' Tests for equality of proportions
#'
#' Run tests for equality of proportions


prop_test <- function(x, n, p = NULL, method = c("wald", "wilson", "agresti-couli", "jeffreys",
                                                 "modified wilson", "wilsoncc", "modified jeffreys",
                                                 "clopper-pearson", "arcsine", "logit", "witting", "pratt"),
                      alternative = c("two.sided", "less", "greater"),
                      conf.level = 0.95, correct = FALSE, exact = FALSE) {

  method <- match.arg(method)
  alternative <- match.arg(alternative)

  if (!is.null(p)) {
    p <- p
  } else {
    p <- 0.5
  }

  exact_test <- binom.test(x, n, alternative = alternative, conf.level = conf.level)
  if (exact == TRUE) {
    exact_p <- exact_test$p.value
  } else {
    exact_p <- NULL
  }
  exact_ci <- exact_test$conf.int[1:2]

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

    ci <- DescTools::BinomCI(x, n, method = "wald", conf.level = conf.level)
    method_ci <- ci[1, 2:3]

  } else if (method == "wilson") {

    ci <- DescTools::BinomCI(x, n, method = "wilson", conf.level = conf.level)
    method_ci <- ci[1, 2:3]

  } else if (method == "agresti-couli") {

    ci <- DescTools::BinomCI(x, n, method = "agresti-couli", conf.level = conf.level)
    method_ci <- ci[1, 2:3]

  } else if (method == "jeffreys") {

    ci <- DescTools::BinomCI(x, n, method = "jeffreys", conf.level = conf.level)
    method_ci <- ci[1, 2:3]

  } else if (method == "modified wilson") {

    ci <- DescTools::BinomCI(x, n, method = "modified wilson", conf.level = conf.level)
    method_ci <- ci[1, 2:3]

  } else if (method == "wilsoncc") {

    ci <- DescTools::BinomCI(x, n, method = "wilsoncc", conf.level = conf.level)
    method_ci <- ci[1, 2:3]

  } else if (method == "modified jeffreys") {

    ci <- DescTools::BinomCI(x, n, method = "modified jeffreys", conf.level = conf.level)
    method_ci <- ci[1, 2:3]

  } else if (method == "clopper-pearson") {

    ci <- DescTools::BinomCI(x, n, method = "clopper-pearson", conf.level = conf.level)
    method_ci <- ci[1, 2:3]

  } else if (method == "arcsine") {

    ci <- DescTools::BinomCI(x, n, method = "arcsine", conf.level = conf.level)
    method_ci <- ci[1, 2:3]

  } else if (method == "logit") {

    ci <- DescTools::BinomCI(x, n, method = "logit", conf.level = conf.level)
    method_ci <- ci[1, 2:3]

  } else if (method == "witting") {

    ci <- DescTools::BinomCI(x, n, method = "witting", conf.level = conf.level)
    method_ci <- ci[1, 2:3]

  } else if (method == "pratt") {

    ci <- DescTools::BinomCI(x, n, method = "pratt", conf.level = conf.level)
    method_ci <- ci[1, 2:3]

  }

  out <- list(x = x, n = n, p = p, estimate = estimate, method = method, method_ci = method_ci,
              exact_ci = exact_ci, exact_p = exact_p, statistic = statistic, df = df, p_value = p_value)
  class(out) <- "prop_test"
  print(out)
}


