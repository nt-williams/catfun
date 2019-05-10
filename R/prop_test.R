#' Tests for equality of proportions
#'
#' Conduct 1-sample tests of proportions and tests for equality of k proportions.
#'
#' @param x a vector of counts, a one-dimensional table with two entries, or a two-dimensional table with 2 columns. Used to select method.
#' @param ... further arguments passed to or from other methods.
#' @param n a vector of counts of trials, ignored if x is a matrix or table.
#' @param p a probability for the null hypothesis when testing a single proportion; ignored if comparing multiple proportions.
#' @param pred predictor/exposure, vector. Must be blank if x is a table or matrix.
#' @param out outcome, vector. Must be blank if x is a table or matrix.
#' @param weight an optional vector of count weights. Must be blank if x is a table or matrix.
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
#' prop_test(c(23, 24), c(50, 55))
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
#' prop_test(vietnam, service, sleep, count)
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
#' @importFrom stats binom.test pnorm prop.test qnorm xtabs
#' @export
prop_test <- function(x, ...) UseMethod("prop_test")

#' @inheritParams prop_test
#' @export
#' @rdname prop_test
prop_test.default <- function(x, ...) {
  argus <- list(...)
  to_print <- list(x = x, n = argus$n, p = argus$p, estimate = argus$estimate,
                   method = argus$method, method_ci = argus$method_ci,
                   exact_ci = argus$exact_ci, exact_p = argus$exact_p,
                   statistic = argus$statistic, df = argus$df, p_value = argus$p_value)
  class(to_print) <- "prop_test"
  to_print
}

#' @inheritParams prop_test
#' @export
#' @rdname prop_test
prop_test.data.frame <- function(x, pred, out, weight = NULL, rev = c("neither", "rows", "columns", "both"),
                                 method = c("wald", "wilson", "agresti-couli", "jeffreys",
                                            "modified wilson", "wilsoncc", "modified jeffreys",
                                            "clopper-pearson", "arcsine", "logit", "witting", "pratt"),
                                 alternative = c("two.sided", "less", "greater"),
                                 conf.level = 0.95, correct = FALSE, exact = FALSE, ...) {
  pred <- rlang::enexpr(pred)
  outc <- rlang::enexpr(out)
  weight <- rlang::enexpr(weight)
  rev <- match.arg(rev)
  tab <- tavolo(df = x, x = !!pred, y = !!outc, weight = !!weight, rev = rev)

  prop_test.table(x = tab, method = method, alternative = alternative,
                  conf.level = conf.level, correct = correct, exact = exact)
}

#' @inheritParams prop_test
#' @export
#' @rdname prop_test
prop_test.numeric <- function(x, n, p = .5,
                              method = c("wald", "wilson", "agresti-couli", "jeffreys",
                                         "modified wilson", "wilsoncc", "modified jeffreys",
                                         "clopper-pearson", "arcsine", "logit", "witting", "pratt"),
                              alternative = c("two.sided", "less", "greater"),
                              conf.level = 0.95, correct = FALSE, exact = FALSE, ...) {
  method <- match.arg(method)
  alternative <- match.arg(alternative)

  if (length(x) == 1L) {
    exact_test <- binom.test(x, n, p = p, alternative = alternative, conf.level = conf.level)
    if (exact == TRUE) exact_p <- round(exact_test$p.value, 5L)
    else exact_p <- NULL
    exact_ci <- exact_test$conf.int[1L:2L]
  } else if (length(x) > 1L) {
    p <- NULL
    exact_ci <- NULL
    exact_p <- NULL
  }

  if (correct == FALSE) test <- prop.test(x = x, n = n, p = p, alternative = alternative,
                                          conf.level = conf.level, correct = FALSE)
  else {
    test <- prop.test(x = x, n = n, p = p, alternative = alternative, conf.level = conf.level, correct = TRUE)
    method <- "wilsoncc"
  }

  statistic <- test$statistic
  df <- test$parameter
  p_value <- round(test$p.value, 5L)
  estimate <- round(test$estimate, 4L)
  ci <- DescTools::BinomCI(x, n, method = method, conf.level = conf.level)
  method_ci <- ci[, 2L:3L]

  if (length(x) > 1L) {
    rownames(method_ci) <- c()
    method_ci <- as.data.frame(method_ci)
    names(method_ci) <- c("Lower bound", "Upper bound")
  }

  prop_test.default(x = x, n = n, p = p, estimate = estimate, method = method,
                    method_ci = method_ci, exact_ci = exact_ci, exact_p = exact_p,
                    statistic = statistic, df = df, p_value = p_value)
}

#' @inheritParams prop_test
#' @export
#' @rdname prop_test
prop_test.table <- function(x, method = c("wald", "wilson", "agresti-couli", "jeffreys",
                                       "modified wilson", "wilsoncc", "modified jeffreys",
                                       "clopper-pearson", "arcsine", "logit", "witting", "pratt"),
                            alternative = c("two.sided", "less", "greater"),
                            conf.level = 0.95, correct = FALSE, exact = FALSE, ...) {
  method <- match.arg(method)
  alternative <- match.arg(alternative)
  n <- rowSums(x)
  x <- x[, 1L]
  p <- NULL
  exact_ci <- NULL
  exact_p <- NULL

  if (correct == FALSE) {
    test <- prop.test(x = x, n = n, p = p, alternative = alternative, conf.level = conf.level, correct = FALSE)
  } else {
    test <- prop.test(x = x, n = n, p = p, alternative = alternative, conf.level = conf.level, correct = TRUE)
    method <- "wilsoncc"
  }

  statistic <- test$statistic
  df <- test$parameter
  p_value <- round(test$p.value, 5L)
  estimate <- round(test$estimate, 4L)
  ci <- DescTools::BinomCI(x, n, method = method, conf.level = conf.level)
  method_ci <- ci[, 2L:3L]
  rownames(method_ci) <- c()
  method_ci <- as.data.frame(method_ci)
  names(method_ci) <- c("Lower bound", "Upper bound")

  prop_test.default(x = x, n = n, p = p, estimate = estimate, method = method,
                    method_ci = method_ci, exact_ci = exact_ci, exact_p = exact_p,
                    statistic = statistic, df = df, p_value = p_value)
}

#' @inheritParams prop_test
#' @export
#' @rdname prop_test
prop_test.matrix <- function(x, n, p = NULL,
                             method = c("wald", "wilson", "agresti-couli", "jeffreys",
                                        "modified wilson", "wilsoncc", "modified jeffreys",
                                        "clopper-pearson", "arcsine", "logit", "witting", "pratt"),
                             alternative = c("two.sided", "less", "greater"),
                             conf.level = 0.95, correct = FALSE, exact = FALSE, ...) {

  method <- match.arg(method)
  alternative <- match.arg(alternative)
  n <- rowSums(x)
  x <- x[, 1L]
  exact_ci <- NULL
  exact_p <- NULL

  if (correct == FALSE) test <- prop.test(x = x, n = n, p = p, alternative = alternative,
                                          conf.level = conf.level, correct = FALSE)
  else {
    test <- prop.test(x = x, n = n, p = p, alternative = alternative, conf.level = conf.level, correct = TRUE)
    method <- "wilsoncc"
  }

  statistic <- test$statistic
  df <- test$parameter
  p_value <- round(test$p.value, 5L)
  estimate <- round(test$estimate, 4L)
  ci <- DescTools::BinomCI(x, n, method = method, conf.level = conf.level)
  method_ci <- ci[, 2L:3L]
  rownames(method_ci) <- c()
  method_ci <- as.data.frame(method_ci)
  names(method_ci) <- c("Lower bound", "Upper bound")

  prop_test.default(x = x, n = n, p = p, estimate = estimate, method = method,
                    method_ci = method_ci, exact_ci = exact_ci, exact_p = exact_p,
                    statistic = statistic, df = df, p_value = p_value)
}

#' @inheritParams prop_test
#' @export
#' @rdname prop_test
print.prop_test <- function(x, ...) {
  if (length(x$x) == 1L) {
    cli::cat_line()
    cat(x$x, " out of ", paste0(x$n, ","), " null probability = ", x$p, "\n")
    cat(paste(rep("-", 40L), collapse = ""), "\n")
    cat("Observed proportion:", x$estimate, "\n")
    cat("Confidence interval method:", x$method, "\n")
    cat("Confidence interval:", x$method_ci, "\n")
    cat("\n")
    cat("Exact limits:", x$exact_ci, "\n")
    cat("\n")
    cat("Test that", x$estimate, "=", x$p, "\n")
    cat(paste(rep("-", 40L), collapse = ""), "\n")
    cat("Chi-squared:", x$statistic, "\n")
    cat("Degrees freedom:", x$df, "\n")
    cat("p-value:", x$p_value, "\n")
    if (!is.null(x$exact_p)) {
      cat("Exact p-value:", x$exact_p, "\n")
    }
  } else {
    cli::cat_line()
    cat("Observed proportions:", "\n")
    cli::cat_bullet(x$estimate)
    cat(paste(rep("-", 40L), collapse = ""), "\n")
    cat("Confidence interval method:", x$method, "\n")
    cat("Confidence intervals:", "\n")
    print(x$method_ci)
    cat(paste(rep("-", 40L), collapse = ""), "\n")
    cat(length(x$x), "sample test for equality of proportions", "\n")
    cat("Chi-squared:", x$statistic, "\n")
    cat("p-value:", x$p_value, "\n")
    cli::cat_line()
  }
}
