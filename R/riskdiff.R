
#' Risk difference
#'
#' Calculate risk difference and 95 percent confidence interval using Wald method


riskdiff <- function(df, x, y = NULL, weight = NULL, conf.level = 0.95,
                     rev = c("neither", "rows", "columns", "both"),
                     dnn = NULL) {

  # error checks
  if (is.matrix(x) && !is.null(y)) {
    stop("If x is a matrix, y should be NULL")
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
  p1 <- x[1,1] / sum(x[, 1])
  p2 <- x[1, 2] / sum(x[, 2])
  rd <- round(p1 - p2, 4)
  z <- qnorm(0.5 * (1 + conf.level))
  se <- sqrt((p1*(1 - p1)/sum(x[, 1])) + (p2*(1 - p2)/sum(x[, 2])))
  if (rd < 0)
    ci <- rd + c(-1, 1) * z * se
  else if (rd > 0)
    ci <- rd + c(1, -1) * z * se

  out <- list(rd = rd, conf.level = conf.level, ci = ci,
              p1 = p1, p2 = p2, tab = x)
  class(out) <- "rdiff"
  out

}





