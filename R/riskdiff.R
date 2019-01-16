
#' Risk difference
#'
#' Calculate risk difference and 95 percent confidence interval using Wald method
#'
#' @param df a one-dimensional dataframe containing counts or a two-dimensional dataframe/matrix of counts.
#'      If a matrix, x and y must be NULL
#' @param x a vector of counts or a 2x2 matrix of counts. If a matrix, df and y must be NULL
#' @param y an optional vector of counts
#' @param weight an optional vector of count weights
#' @param conf.level confidence level for confidence interval, default is 0.95
#' @param rev reverse order cells. Options are "row", "columns", "both", and "neither" (default)
#' @param dnn optional character vector of dimension names
#'
#' @return an S3 object
#'
#' @examples
#' trial <- data.frame(
#'   disease = c(rep("yes", 2), rep("no", 2)),
#'   treatment = c(rep(c("estrogen", "placebo"), 2)),
#'   count = c(751, 623, 7755, 7479))
#'
#' riskdiff(trial, "treatment", "disease", weight = "count")
#'
#' @importFrom magrittr %>%
#' @importFrom epitools epitable
#' @importFrom stats binom.test pnorm prop.test qnorm xtabs
#' @export


riskdiff <- function(df, x = NULL, y = NULL, weight = NULL, conf.level = 0.95,
                     rev = c("neither", "rows", "columns", "both"),
                     dnn = NULL) {

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

  # extracting dimension names
  if (is.null(dnn)) {
    if (!is.null(y))
      if (missing(df))
        dnn <- c(deparse(substitute(x)), deparse(substitute(y)))
      else dnn <- c(noquote(x), noquote(y))
      else if (is.null(y))
        dnn <- names(dimnames(x))
  } else dnn <- dnn

  if (is.matrix(df)) {
    if (ncol(df) != 2L) {
      stop("If using a matrix, dimensions must be 2x2")
    } else if (nrow(df) != 2L) {
      stop("If using a matrix, dimensions must be 2x2")
    }

    if (!is.null(x)) {
      stop("If df is a matrix, x should be NULL")
    } else if (!is.null(y)) {
      stop("If df is a matrix, y should be NULL")
    } else {
      x <- epitable(df, rev = rev)
    }
  }

  if (missing(df)) {
    if (is.matrix(x)) {
      x <- epitable(x, rev = rev)
    }

    if (!is.null(y) && is.null(weight)) {
      # x <- xtabs(~ x + y) %>%
      #   epitable(rev = rev)
      x <- table(x, y) %>%
        epitable(rev = rev)
    } else if (!is.null(y) && !is.null(weight)) {
      x <- xtabs(weight ~ x + y) %>%
        epitable(rev = rev)
    }
  } else {
    if (!is.null(y) && is.null(weight)) {
      # x <- xtabs(~ df[[x]] + df[[y]]) %>%
      #   epitable(rev)

      x <- table(df[[x]], df[[y]]) %>%
        epitable(rev = rev)

    } else if (!is.null(y) && !is.null(weight)) {

      x <- xtabs(df[[weight]] ~ df[[x]] + df[[y]]) %>%
        epitable(rev = rev)
    }
  }

  names(dimnames(x)) <- dnn

  # rd and conf interval
  p1 <- x[1L, 1L] / sum(x[1L, ])
  p2 <- x[2L, 1L] / sum(x[2L, ])
  rd <- round(p1 - p2, 4L)
  z <- qnorm(0.5 * (1L + conf.level))
  se <- sqrt((p1*(1L - p1)/sum(x[1L, ])) + (p2*(1L - p2)/sum(x[2L, ])))
  if (rd < 0L)
    ci <- rd + c(1L, -1L) * z * se
  else if (rd > 0)
    ci <- rd + c(-1L, 1L) * z * se

  out <- list(rd = rd, conf.level = conf.level, ci = ci,
              p1 = p1, p2 = p2, tab = x)
  class(out) <- "rdiff"
  out

}





