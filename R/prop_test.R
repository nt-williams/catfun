
#' Tests for equality of proportions
#'
#' Run tests for equality of proportions
#'
#' @param x a vector of counts, a one-dimensional table with two entries, or a two-dimensional table with 2 columns.
#' @param n a vector of counts of trials, ignored if x is a matrix or table.
#' @param p a vector of probabilities, length must be same as the number of groups specified by x.
#' @param method a character string indicating method for calculating confidence interval, default is "wald".
#' @param alternative character string specifiying the alternative hypothesis. Possible options are "two.sided" (default),
#'      "greater", or "less".
#' @param conf.level confidence level for confidence interval, default is 0.95.
#' @param correct a logical indicating whether Yate's continuity correction should be applied.
#' @param exact a logical indicating whether to output exact p-value, ignored if k-sample test.
#'
#' @examples
#' prop_test(7, 50, method = "wald", p = 0.2)
#' prop_test(7, 50, method = "wald", p = 0.2, exact = TRUE)
#'
#' vietnam <- data.frame(
#'    service = c(rep("yes", 2), rep("no", 2)),
#'    sleep = c(rep(c("yes", "no"), 2)),
#'    count = c(173, 160, 599, 851)
#' )
#'
#' sleep <- xtabs(count ~ service + sleep, data = vietnam)
#' prop_test(sleep)
#'
#' @return a list with class "prop_test" containing the following components:
#'
#' \item{x}{number of successes}
#' \item{n}{number of trials}
#' \item{p}{null proportion}
#' \item{statistic}{the value of Pearson's chi-squared test statistic}
#' \item{p_value}{p-value corresponding to chi-squared test statistic}
#' \item{df}{degrees of freedom}
#' \item{method}{the method used to calculate the confidence interval}
#' \item{method_ci}{confidence interval calculated using specified method}
#' \item{exact_ci}{exact confidence interval}
#' \item{exact_p}{p-value from exact test}
#'
#' @importFrom magrittr %>%
#' @importFrom stats binom.test pnorm prop.test qnorm xtabs
#' @export


prop_test <- function(x, n, p = NULL, method = c("wald", "wilson", "agresti-couli", "jeffreys",
                                                 "modified wilson", "wilsoncc", "modified jeffreys",
                                                 "clopper-pearson", "arcsine", "logit", "witting", "pratt"),
                      alternative = c("two.sided", "less", "greater"),
                      conf.level = 0.95, correct = FALSE, exact = FALSE) {

  method <- match.arg(method)
  alternative <- match.arg(alternative)

  if (is.matrix(x)) {
    n <- rowSums(x)
    x <- x[, 1L]
  }

  if (length(x) == 1L) {
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
    exact_ci <- exact_test$conf.int[1L:2L]
  } else if (length(x) > 1L && is.null(p)) {
    exact_ci <- NULL
    exact_p <- NULL
  }

  if (correct == FALSE) {
    test <- prop.test(x = x, n = n, p = p, alternative = alternative, correct = FALSE)
  } else {
    test <- prop.test(x = x, n = n, p = p, alternative = alternative, correct = TRUE)
    method <- "wilsoncc"
  }

  statistic <- test$statistic
  df <- test$parameter
  p_value <- test$p.value
  estimate <- round(test$estimate, 4L)

  if (method == "wald") {

    ci <- DescTools::BinomCI(x, n, method = "wald", conf.level = conf.level)
    method_ci <- ci[, 2L:3L]

  } else if (method == "wilson") {

    ci <- DescTools::BinomCI(x, n, method = "wilson", conf.level = conf.level)
    method_ci <- ci[, 2L:3L]

  } else if (method == "agresti-couli") {

    ci <- DescTools::BinomCI(x, n, method = "agresti-couli", conf.level = conf.level)
    method_ci <- ci[, 2L:3L]

  } else if (method == "jeffreys") {

    ci <- DescTools::BinomCI(x, n, method = "jeffreys", conf.level = conf.level)
    method_ci <- ci[, 2L:3L]

  } else if (method == "modified wilson") {

    ci <- DescTools::BinomCI(x, n, method = "modified wilson", conf.level = conf.level)
    method_ci <- ci[, 2L:3L]

  } else if (method == "wilsoncc") {

    ci <- DescTools::BinomCI(x, n, method = "wilsoncc", conf.level = conf.level)
    method_ci <- ci[, 2L:3L]

  } else if (method == "modified jeffreys") {

    ci <- DescTools::BinomCI(x, n, method = "modified jeffreys", conf.level = conf.level)
    method_ci <- ci[, 2L:3L]

  } else if (method == "clopper-pearson") {

    ci <- DescTools::BinomCI(x, n, method = "clopper-pearson", conf.level = conf.level)
    method_ci <- ci[, 2L:3L]

  } else if (method == "arcsine") {

    ci <- DescTools::BinomCI(x, n, method = "arcsine", conf.level = conf.level)
    method_ci <- ci[, 2L:3L]

  } else if (method == "logit") {

    ci <- DescTools::BinomCI(x, n, method = "logit", conf.level = conf.level)
    method_ci <- ci[, 2L:3L]

  } else if (method == "witting") {

    ci <- DescTools::BinomCI(x, n, method = "witting", conf.level = conf.level)
    method_ci <- ci[, 2L:3L]

  } else if (method == "pratt") {

    ci <- DescTools::BinomCI(x, n, method = "pratt", conf.level = conf.level)
    method_ci <- ci[, 2L:3L]

  }

  if (length(x) > 1L) {
    rownames(method_ci) <- c()
    method_ci <- as.data.frame(method_ci)
    names(method_ci) <- c("Lower bound", "Upper bound")
  }

  out <- list(x = x, n = n, p = p, estimate = estimate, method = method, method_ci = method_ci,
              exact_ci = exact_ci, exact_p = exact_p, statistic = statistic, df = df, p_value = p_value)
  class(out) <- "prop_test"
  out
}


