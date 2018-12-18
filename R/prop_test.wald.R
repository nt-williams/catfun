
prop_test.wald <- function(x, n, p = NULL,
                           alternative = c("two.sided", "less", "greater"),
                           conf.level = 0.95, correct = FALSE) {

  alternative <- match.arg(alternative)

  method <- "1 sample test of proportions without continuity correction"
  d_name <- paste(x, "out of", n)

  p_mle <- x / n
  names(p_mle) <- "Proportion"

  se_ci <- sqrt(p_mle * (1 - p_mle) / n)
  z_cv <- qnorm(0.5 * (1 + conf.level))
  ci <- p_mle + c(-1, 1) * z_cv * se_ci
  attr(ci, "conf.level") <- conf.level

  if (!is.null(p)) {
    se <- sqrt(p * (1 - p) / n)
    statistic <- (p_mle - p) / se
  } else {
    p <- 0.5
    se <- sqrt(p * (1 - p) / n)
    statistic <- (p_mle - p) / se
  }

  if (any(n*p < 5, n*p*(1 - p) < 5)) {
    warning("Normal approximation may be incorrect. Consider using exact method")
  }

  names(statistic) <- "Z"
  names(p) <- "proportion"

  if (alternative == "two.sided") {
    if (statistic < 0) {
      p.value <- pnorm(statistic) * 2
    } else {
      p.value <- pnorm(statistic, lower.tail = FALSE) * 2
    }
  } else if (alternative == "less") {
    p.value <- pnorm(statistic)
  } else if (alternative == "greater") {
    p.value <- pnorm(statistic, lower.tail = FALSE)
  }

  vals <- list(null.value = p, alternative = alternative, method = method,
               estimate = p_mle, data.name = d_name, statistic = statistic,
               p.value = p.value, conf.int = ci)
  class(vals) <- "htest"
  print(vals)
}
