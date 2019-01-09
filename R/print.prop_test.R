
#' @importFrom cli cat_bullet
#' @importFrom cli cat_rule
#' @importFrom cli cat_line
#' @export

print.prop_test <- function(z, ...) {
  if (length(z$x) == 1) {
    cat(z$x, " out of ", paste0(z$n, ","), " null probability = ", z$p)
    cat("\n")
    cat(paste(rep("-", 40), collapse = ""), "\n")
    cat("Observed proportion:", z$estimate, "\n")
    cat("Confidence interval method:", z$method, "\n")
    cat("Confidence interval:", z$method_ci, "\n")
    cat("\n")
    cat("Exact limits:", z$exact_ci, "\n")
    cat("\n")
    cat("Test that", z$estimate, "=", z$p, "\n")
    cat(paste(rep("-", 40), collapse = ""), "\n")
    cat("Chi-squared:", z$statistic, "\n")
    cat("Degrees freedom:", z$df, "\n")
    cat("p-value:", z$p_value, "\n")
    cat("\n")
    if (!is.null(z$exact_p)) {
      cat("Exact p-value:", z$exact_p, "\n")
    }
    cat("\n")
  } else {
    cat("Observed proportions:", "\n")
    cat_bullet(z$estimate)
    cat_line()
    cat("Confidence interval method:", z$method, "\n")
    cat("Confidence intervals:", "\n")
    print(z$method_ci)
    cat_rule()
    cat(length(z$x), "sample test for equality of proportions", "\n")
    cat("Chi-squared:", z$statistic, "\n")
    cat("p-value:", z$p_value)
  }

}

