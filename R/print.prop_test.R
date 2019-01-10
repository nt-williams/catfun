
#' @importFrom cli cat_bullet cat_rule cat_line
#' @export

print.prop_test <- function(x, ...) {
  if (length(x$x) == 1) {
    cat(x$x, " out of ", paste0(x$n, ","), " null probability = ", x$p)
    cat("\n")
    cat(paste(rep("-", 40), collapse = ""), "\n")
    cat("Observed proportion:", x$estimate, "\n")
    cat("Confidence interval method:", x$method, "\n")
    cat("Confidence interval:", x$method_ci, "\n")
    cat("\n")
    cat("Exact limits:", x$exact_ci, "\n")
    cat("\n")
    cat("Test that", x$estimate, "=", x$p, "\n")
    cat(paste(rep("-", 40), collapse = ""), "\n")
    cat("Chi-squared:", x$statistic, "\n")
    cat("Degrees freedom:", x$df, "\n")
    cat("p-value:", x$p_value, "\n")
    cat("\n")
    if (!is.null(x$exact_p)) {
      cat("Exact p-value:", x$exact_p, "\n")
    }
    cat("\n")
  } else {
    cat("Observed proportions:", "\n")
    cat_bullet(x$estimate)
    cat_line()
    cat("Confidence interval method:", x$method, "\n")
    cat("Confidence intervals:", "\n")
    print(x$method_ci)
    cat_rule()
    cat(length(x$x), "sample test for equality of proportions", "\n")
    cat("Chi-squared:", x$statistic, "\n")
    cat("p-value:", x$p_value)
  }

}

