
prop_test <- function(x, n, p = NULL, method = c("wald", "wilson"),
                      alternative = c("two.sided", "less", "greater"),
                      conf.level = 0.95, correct = FALSE) {

  method <- match.arg(method)
  alternative <- match.arg(alternative)
  parameter <- 1
  names(parameter) <- "df"

  if (method == "wald") {
    if (correct) {
      stop("Nick hasn't gotten this far yet")
    } else {
      method <- "1 sample test of proportions without continuity correction"

      p_mle <- x / n
      names(p_mle) <- "MLE proportion"

      se <- sqrt(p_mle * (1 - p_mle) / n)
      z_cv <- qnorm(0.5 * (1 + conf.level))
      ci <- round(p_mle + c(-1, 1) * z_cv * se, 4)
      attr(ci, "conf.level") <- conf.level

      if (!is.null(p)) {
        statistic <- (p_mle - p) / se
      } else {
        p <- 0.5
        statistic <- (p_mle - p) / se

      }
      names(statistic) <- "Chi-squared"
      names(p) <- "proportion"

      # if (alternative == "two.sided") {
      #
      # } else if (alternative == "less") {
      #
      # } else if (alternative == "greater") {
      #
      # }
    }
    vals <- list(null.value = p, alternative = alternative, method = method,
                 estimate = p_mle, data.name = "Test", statistic = statistic,
                 parameter = parameter, p.value = 0.5, conf.int = ci)
    class(vals) <- "htest"
    print(vals)

  } else if (method == "wilson") {
    prop.test(x = x, n = n, p = p, alternative = alternative,
              conf.level = conf.level, correct = correct)
  }
}
