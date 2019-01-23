
#' @importFrom cli cat_bullet cat_rule cat_line
#' @export

print.prop_power <- function(x, ...) {
  cat_line()
  cat("\t", "Two-sample comparison of proportions power calculation", "\n")
  cat_rule()
  cat_line()
  cat("Total sample size:", x$n, "\n")
  cat("N1:", x$n1, "\n")
  cat("N2:", x$n2, "\n")
  cat("Proportion 1:", x$p1, "\n")
  cat("Proportion 2:", x$p2, "\n")
  cat("Power:", x$power, "\n")
  cat("Significance level:", x$sig.level, "\n")
  cat_line()
}
