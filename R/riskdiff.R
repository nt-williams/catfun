
riskdiff <- function(x, y = NULL, weight = NULL, conf.level = 0.95,
                     rev = c("neither", "rows", "columns", "both"),
                     dnn = NULL) {

  # error checks
  if (is.matrix(x) && !is.null(y)) {
    stop("If x is a matrix, y should be NULL")
  }

  # extracting dimension names
  if (is.null(dnn))
    if (!is.null(y))
      dnn <- c(deparse(substitute(x)), deparse(substitute(y)))
    else if (is.null(y))
      dnn <- names(dimnames(x))
    else (!is.null(dnn))
    dnn <- dnn

    # building table
    if (is.matrix(x)) {
      x <- epitable(x, rev = rev)
    }
    if (!is.null(y) && is.null(weight)) {
      x <- table(x, y) %>%
        epitable(rev = rev)
    } else if (!is.null(y) && !is.null(weight)) {
      x <- xtabs(weight ~ x + y) %>%
        epitable(rev = rev)
    }

    names(dimnames(x)) <- dnn

    # rd and conf interval
    p1 <- x[1,1] / sum(x[, 1])
    p2 <- x[1, 2] / sum(x[, 2])
    rd <- round(dge - ndge, 4)
    z <- qnorm(0.5 * (1 + conf.level))
    se <- sqrt((p1*(1 - p1)/sum(x[, 1])) + (p2*(1 - p2)/sum(x[, 2])))
    if (rd < 0)
      ci <- rd + c(-1, 1) * z * se
    else if (rd > 0)
      ci <- rd + c(1, -1) * z * se

    cat("Risk difference:", rd, "\n")
    cat(paste(conf.level * 100, "%", sep = ""), "confidence interval:", round(ci, 4), "\n")
    cat("\n")
    cat("Contingency table: \n")
    x

}