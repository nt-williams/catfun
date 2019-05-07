#' Risk difference
#'
#' Calculate risk difference and 95 percent confidence interval using Wald method.
#'
#' @param df a one-dimensional dataframe containing counts, a two-dimensional dataframe with binary variables x and y, or matrix of counts.
#'      If a matrix, x and y must be NULL
#' @param x predictor/exposure, vector.
#' @param y outcome, vector.
#' @param weight an optional vector of count weights.
#' @param conf.level confidence level for confidence interval, default is 0.95.
#' @param rev reverse order of cells. Options are "row", "columns", "both", and "neither" (default).
#' @param dnn optional character vector of dimension names.
#'
#' @return a list with class "rdiff" containing the following components:
#'
#' \item{rd}{risk difference}
#' \item{conf.level}{specified confidence level}
#' \item{ci}{calculated confidence interval}
#' \item{p1}{proportion one}
#' \item{p2}{proportion two}
#' \item{tab}{2x2 table using for calculating risk difference}
#'
#' @examples
#' trial <- data.frame(
#'   disease = c(rep("yes", 2), rep("no", 2)),
#'   treatment = c(rep(c("estrogen", "placebo"), 2)),
#'   count = c(751, 623, 7755, 7479))
#'
#' riskdiff(trial, treatment, disease, count, rev = "columns")
#'
#' @importFrom stats binom.test pnorm prop.test qnorm xtabs
#' @export
riskdiff <- function(df, x = NULL, y = NULL, weight = NULL, conf.level = 0.95,
                     rev = c("neither", "rows", "columns", "both"), dnn = NULL) {

  x <- rlang::enexpr(x)
  y <- rlang::enexpr(y)
  weight <- rlang::enexpr(weight)

  # error checks
  if (is.matrix(x) && !is.null(y)) {
    stop("If x is a matrix, y should be NULL")
  } else if (is.matrix(x) && is.null(y)) {
    if (ncol(x) != 2L) {
      stop("If using a matrix, dimensions must be 2x2")
    } else if (nrow(x) != 2L) {
      stop("If using a matrix, dimensions must be 2x2")
    }
  }

  # frequency table
  tab <- tobyto(df = df, x = x, y = y, weight = weight, rev = rev, dnn = dnn)

  # rd and conf interval
  p1 <- tab[1L, 1L] / sum(tab[1L, ])
  p2 <- tab[2L, 1L] / sum(tab[2L, ])
  rd <- round(p1 - p2, 4L)
  z <- qnorm(0.5 * (1L + conf.level))
  se <- sqrt((p1*(1L - p1)/sum(tab[1L, ])) + (p2*(1L - p2)/sum(tab[2L, ])))

  if (rd < 0L)
    ci <- rd + c(1L, -1L) * z * se
  else if (rd > 0)
    ci <- rd + c(-1L, 1L) * z * se

  if (ci[1L] < -1L) {
    ci[1L] <- -1L
  } else if (ci[2L] > 1L) {
    ci[2L] <- 1L
  }

  out <- list(rd = rd, conf.level = conf.level, ci = ci,
              p1 = p1, p2 = p2, tab = tab)
  class(out) <- "rdiff"
  out

}

#' @inheritParams riskdiff
#' @export
#' @rdname riskdiff
print.rdiff <- function(x, ...) {
  cat("\n")
  cat("Risk difference:", x$rd, "\n")
  cat(paste(x$conf.level * 100L, "%", sep = ""), "confidence interval:", round(x$ci, 4), "\n")
  cat("\n")
  cat("Proportion 1 =", round(x$p1, 4L), "\n")
  cat("Proportion 2 =", round(x$p2, 4L), "\n")
  cat(paste(rep("-", 40L), collapse = ""), "\n")
  cat("\n")
  cat("Frequency table: \n")
  cat("\n")
  print(x$tab)
  cli::cat_line()
}
