
print.rdiff <- function(x, ...) {
  cat("\n")
  cat("Risk difference:", x$rd, "\n")
  cat(paste(x$conf.level * 100, "%", sep = ""), "confidence interval:", round(x$ci, 4), "\n")
  cat("\n")
  cat("Proportion 1 =", round(x$p1, 4), "\n")
  cat("Proportion 2 =", round(x$p2, 4), "\n")
  cat(paste(rep("-", 45), collapse = ""), "\n")
  cat("\n")
  cat("Contingency table: \n")
  cat("\n")
  print(x$tab)
}


