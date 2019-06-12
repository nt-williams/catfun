#' Matched pairs odds ratio and confidence interval
#'
#' Create odds ratio and confidence interval from matched pairs data.
#'
#' @param df a dataframe with binary variables x and y or a 2 x 2 frequency table/matrix. If a table or matrix, x and y must be NULL. Used to select method.
#' @param ... further arguments passed to or from other methods.
#'
#' @return a list with class "matched_or" with the following components:
#'
#' \item{tab}{2x2 table using for calculating risk difference}
#' \item{or}{dataframe with columns corresponding to matched-pairs OR, lower bound, and upper bound of CI}
#' \item{conf.level}{specified confidence level}
#' @export
#'
#' @examples
#'
#' gene <- data.frame(pair = seq(1:35),
#'                    ulcer = rbinom(35, 1, .7),
#'                    healthy = rbinom(35, 1, .4))
#'
#' matched_or(gene, ulcer, healthy)
matched_or <- function(df, ...) UseMethod("matched_or")

#' @export
matched_or.default <- function(df, ...) {
  argus <- list(...)
  to_print <- list(tab = df, or = argus$or, conf.level = argus$conf.level)
  class(to_print) <- "matched_or"
  to_print
}

#' Matched pairs odds ratio from a data frame
#'
#' #' Create odds ratio and confidence interval from matched pairs data.
#'
#' @param df a dataframe with binary variables x and y.
#' @param x binary vector, used as rows for frequency table and calculations.
#' @param y binary vector, used as columns for frequency table and calculations.
#' @param weight an optional vector of count weights.
#' @param alpha level of significance for confidence interval.
#' @param rev reverse order of cells. Options are "row", "columns", "both", and "neither" (default).
#' @param ... further arguments passed to or from other methods.
#'
#' @return a list with class "matched_or" with the following components:
#'
#' \item{tab}{2x2 table using for calculating risk difference}
#' \item{or}{dataframe with columns corresponding to matched-pairs OR, lower bound, and upper bound of CI}
#' \item{conf.level}{specified confidence level}
#' @export
#'
#' @examples
#'
#' gene <- data.frame(pair = seq(1:35),
#'                    ulcer = rbinom(35, 1, .7),
#'                    healthy = rbinom(35, 1, .4))
#'
#' matched_or.data.frame(gene, ulcer, healthy)
matched_or.data.frame <- function(df, x, y, weight = NULL,
                                 alpha = 0.05, rev = c("neither", "rows", "columns", "both"), ...) {
  pred <- rlang::enexpr(x)
  outc <- rlang::enexpr(y)
  weight <- rlang::enexpr(weight)
  rev <- match.arg(rev)
  tab <- tavolo(df = df, x = !!pred, y = !!outc, weight = !!weight, rev = rev)

  matched_or.table(df = tab, alpha = alpha)
}

#' Matched pairs odds ratio from a table
#'
#' Create odds ratio and confidence interval from matched pairs data.
#'
#' @param df a dataframe with binary variables x and y or a 2 x 2 frequency table/matrix.
#' @param alpha level of significance for confidence interval.
#' @param rev reverse order of cells. Options are "row", "columns", "both", and "neither" (default).
#' @param ... further arguments passed to or from other methods.
#'
#' @return a list with class "matched_or" with the following components:
#'
#' \item{tab}{2x2 table using for calculating risk difference}
#' \item{or}{dataframe with columns corresponding to matched-pairs OR, lower bound, and upper bound of CI}
#' \item{conf.level}{specified confidence level}
#' @export
#'
#' @examples
#' gene <- data.frame(pair = seq(1:35),
#'                    ulcer = rbinom(35, 1, .7),
#'                    healthy = rbinom(35, 1, .4))
#'
#' gene_tab <- xtabs(~ ulcer + healthy, data = gene)
#'
#' gene_tab %>% matched_or.table()
matched_or.table <- function(df, alpha = 0.05, rev = c("neither", "rows", "columns", "both"), ...) {
  rev <- match.arg(rev)

  if (rev == "neither") tab <- df
  else tab <- tavolo(df = df, rev = rev)

  odds_ratio <- df[1, 2] / df[2, 1]
  or_table <- data.frame(
    odds_ratio = odds_ratio,
    lower_bound = odds_ratio *
      exp(qnorm(alpha / 2) * sqrt(1 / df[1, 2] + 1 / df[2, 1])),
    upper_bound = odds_ratio *
      exp(qnorm(alpha / 2, lower.tail = FALSE) * sqrt(1 / df[1, 2] + 1 / df[2, 1]))
  )

  matched_or.default(df = tab, or = or_table, conf.level = 1 - alpha)
}

#' @export
print.matched_or <- function(x, ...) {
  cli::cat_line()
  cat("Frequency table: \n")
  cat("\n")
  print(x$tab)
  cli::cat_line()
  cat(paste(rep("-", 40L), collapse = ""), "\n")
  cli::cat_line()
  cat("Confidence level:", paste(x$conf.level * 100L, "%", sep = ""), "\n")
  print(round(x$or, 4))
  cli::cat_line()
}

