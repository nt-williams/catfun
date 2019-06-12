#' Risk difference
#'
#' Calculate risk difference and 95 percent confidence interval using Wald method.
#'
#' @param df a dataframe with binary variables x and y or a 2 x 2 frequency table/matrix. If a table or matrix, x and y must be NULL. Used to select method.
#' @param ... further arguments passed to or from other methods.
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
riskdiff <- function(df, ...) UseMethod("riskdiff")

#' @export
riskdiff.default <- function(df, conf.level, ...) {
  tab <- df
  p1 <- tab[1L, 1L] / sum(tab[1L, ])
  p2 <- tab[2L, 1L] / sum(tab[2L, ])
  rd <- round(p1 - p2, 4L)
  z <- qnorm(0.5 * (1L + conf.level))
  se <- sqrt((p1*(1L - p1)/sum(tab[1L, ])) + (p2*(1L - p2)/sum(tab[2L, ])))

  if (rd < 0L) ci <- rd + c(1L, -1L) * z * se
  else if (rd > 0L) ci <- rd + c(-1L, 1L) * z * se

  if (ci[1L] < -1L) ci[1L] <- -1L
  else if (ci[2L] > 1L) ci[2L] <- 1L

  to_print <- list(rd = rd, conf.level = conf.level, ci = ci,p1 = p1, p2 = p2, tab = tab)
  class(to_print) <- "rdiff"
  to_print
}

#' Risk difference
#'
#' Calculate risk difference and 95 percent confidence interval using Wald method.
#'
#' @param df a dataframe with binary variables x and y.
#' @param x binary predictor/exposure, vector.
#' @param y binary outcome, vector.
#' @param weight an optional vector of count weights.
#' @param conf.level confidence level for confidence interval, default is 0.95.
#' @param rev reverse order of cells. Options are "row", "columns", "both", and "neither" (default).
#' @param ... further arguments passed to or from other methods.
#'
#' @return a list with class "rdiff" containing the following components:
#'
#' \item{rd}{risk difference}
#' \item{conf.level}{specified confidence level}
#' \item{ci}{calculated confidence interval}
#' \item{p1}{proportion one}
#' \item{p2}{proportion two}
#' \item{tab}{2x2 table using for calculating risk difference}
#' @export
#'
#' @examples
#' trial <- data.frame(
#'   disease = c(rep("yes", 2), rep("no", 2)),
#'   treatment = c(rep(c("estrogen", "placebo"), 2)),
#'   count = c(751, 623, 7755, 7479))
#'
#' riskdiff(trial, treatment, disease, count, rev = "columns")
riskdiff.data.frame <- function(df, x = NULL, y = NULL, weight = NULL, conf.level = 0.95,
                                rev = c("neither", "rows", "columns", "both"), ...) {
  pred <- rlang::enexpr(x)
  outc <- rlang::enexpr(y)

  if (length(levels(df[[pred]])) > 2L) stop("Only binary predictors and outcomes allowed")
  else if (length(levels(df[[outc]])) > 2L) stop("Only binary predictors and outcomes allowed")

  weight <- rlang::enexpr(weight)
  rev <- match.arg(rev)
  tab <- tavolo(df = df, x = !!pred, y = !!outc, weight = !!weight, rev = rev)

  riskdiff.default(df = tab, conf.level = conf.level)
}

#' Risk difference
#'
#' Calculate risk difference and 95 percent confidence interval using Wald method.
#'
#' @param df a 2 x 2 frequency table.
#' @param conf.level confidence level for confidence interval, default is 0.95.
#' @param rev reverse order of cells. Options are "row", "columns", "both", and "neither" (default).
#' @param ... further arguments passed to or from other methods.
#'
#' @return a list with class "rdiff" containing the following components:
#'
#' \item{rd}{risk difference}
#' \item{conf.level}{specified confidence level}
#' \item{ci}{calculated confidence interval}
#' \item{p1}{proportion one}
#' \item{p2}{proportion two}
#' \item{tab}{2x2 table using for calculating risk difference}
#' @export
#'
#' @examples
#' trial <- data.frame(
#'   disease = c(rep("yes", 2), rep("no", 2)),
#'   treatment = c(rep(c("estrogen", "placebo"), 2)),
#'   count = c(751, 623, 7755, 7479))
#'
#' xtabs(count ~ treatment + disease, data = trial) %>% riskdiff.table()
riskdiff.table <- function(df, conf.level = 0.95, rev = c("neither", "rows", "columns", "both"), ...) {
  rev <- match.arg(rev)

  if (nrow(df) > 2L) stop("Only binary predictors and outcomes allowed")
  else if (ncol(df) > 2L) stop("Only binary predictors and outcomes allowed")

  if (rev == "neither") tab <- df
  else tab <- tavolo(df = df, rev = rev)

  riskdiff.default(df = tab, conf.level = conf.level)
}

#' Risk difference
#'
#' Calculate risk difference and 95 percent confidence interval using Wald method.
#'
#' @param df a 2 x 2 frequency matrix.
#' @param conf.level confidence level for confidence interval, default is 0.95.
#' @param dnn optional character vector of dimension names.
#' @param rev reverse order of cells. Options are "row", "columns", "both", and "neither" (default).
#' @param ... further arguments passed to or from other methods.
#'
#' @return a list with class "rdiff" containing the following components:
#'
#' \item{rd}{risk difference}
#' \item{conf.level}{specified confidence level}
#' \item{ci}{calculated confidence interval}
#' \item{p1}{proportion one}
#' \item{p2}{proportion two}
#' \item{tab}{2x2 table using for calculating risk difference}
#' @export
#'
#' @examples
#' matrix(c(12, 45, 69, 15), nrow = 2, ncol = 2) %>%
#'    riskdiff.matrix(dnn = c("New Drug", "Adverse Outcome"))
riskdiff.matrix <- function(df, conf.level = 0.95, dnn = NULL, rev = c("neither", "rows", "columns", "both"), ...) {
  rev <- match.arg(rev)

  if (nrow(df) > 2L) stop("Only binary predictors and outcomes allowed")
  else if (ncol(df) > 2L) stop("Only binary predictors and outcomes allowed")

  if (rev == "neither") tab <- df
  else tab <- tavolo(df = df, rev = rev)

  rname <- c("Exposed", "Unexposed")
  cname <- c("Outcome", "No Outcome")

  if (is.null(dnn)) dimnames(tab) <- list(Exposure = rname, Outcome = cname)
  else {
    dimnames(tab) <- list(rname, cname)
    names(dimnames(tab)) <- dnn
  }

  riskdiff.default(df = tab, conf.level = conf.level)
}

#' @export
print.rdiff <- function(x, ...) {
  cli::cat_line()
  cat("Risk difference:", x$rd, "\n")
  cat(paste(x$conf.level * 100L, "%", sep = ""), "confidence interval:", round(x$ci, 4L), "\n")
  cat("\n")
  cat("Proportion 1 =", round(x$p1, 4L), "\n")
  cat("Proportion 2 =", round(x$p2, 4L), "\n")
  cat(paste(rep("-", 40L), collapse = ""), "\n")
  cat("Frequency table: \n")
  cat("\n")
  print(x$tab)
  cli::cat_line()
}
