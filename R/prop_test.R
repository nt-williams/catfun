
prop_test <- function(x, n, p = NULL, method = c("wald", "wilson"),
                      alternative = c("two.sided", "less", "greater"),
                      conf.level = 0.95, correct = FALSE) {

  method <- match.arg(method)
  alternative <- match.arg(alternative)

  if (method == "wald") {
    prop_test.wald(x = x, n = n, p = p, alternative = alternative,
                   conf.level = conf.level, correct = correct)

  } else if (method == "wilson") {
    prop.test(x = x, n = n, p = p, alternative = alternative,
              conf.level = conf.level, correct = correct)
  }
}
