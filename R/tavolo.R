#' Create 2 x 2 frequency tables
#'
#' @param ... argument calls from \code{riskdiff()}
#' @param df a dataframe with binary variable y and categorical variable x or a 2 x k frequency table/matrix. If a table or matrix, x and y must be NULL. Used to select method.
#' @param x predictor/exposure, vector. Must be blank if df is a table or matrix.
#' @param y outcome, vector. Must be blank if df is a table or matrix.
#' @param weight an optional vector of count weights. Must be blank if df is a table or matrix.
#' @param dnn optional character vector of dimension names; only available if df is a matrix.
#' @param rev character string indicating whether to switch row or column order, possible options are "neither", "rows", "columns", or "both". The default is "neither".
#'
#' @return
#' \item{tab}{2 x 2 frequency table}
#'
#' @examples
#' trial <- data.frame(disease = c(rep("yes", 2), rep("no", 2)),
#'                     treatment = c(rep(c("estrogen", "placebo"), 2)),
#'                     count = c(751, 623, 7755, 7479))
#'
#' tavolo(trial, treatment, disease, count)
#'
#' @export
tavolo <- function(df, ...) UseMethod("tavolo")

tavolo.default <- function(df, ...) {
  x <- df
  rev <- list(...)

  if (rev["rev"] == "neither") x <- x
  else if (rev["rev"] == "rows") x <- x[2L:1L, ]
  else if (rev["rev"] == "columns") x <- x[, 2L:1L]
  else if (rev["rev"] == "both") x <- x[2L:1L, 2L:1L]

  return(x)
}

#' @inheritParams tavolo
#' @export
#' @rdname tavolo
tavolo.data.frame <- function(df, x, y, weight = NULL,
                              rev = c("neither", "rows", "columns", "both"), ...) {
  pred <- rlang::enexpr(x)
  outc <- rlang::enexpr(y)
  weight <- rlang::enexpr(weight)
  rev <- match.arg(rev)

  if (is.null(weight)) {
    x <- xtabs(~ df[[pred]] + df[[outc]])
  } else {
    x <- xtabs(df[[weight]] ~ df[[pred]] + df[[outc]])
  }

  dnn <- c(pred, outc)
  names(dimnames(x)) <- dnn

  tavolo.default(df = x, rev = rev)
}

#' @inheritParams tavolo
#' @export
#' @rdname tavolo
tavolo.matrix <- function(df, dnn = NULL, rev = c("neither", "rows", "columns", "both"), ...) {
  x <- df
  rev <- match.arg(rev)
  rname <- c("Exposed", "Unexposed")
  cname <- c("Outcome", "No Outcome")

  if (is.null(dnn)) dimnames(x) <- list(Exposure = rname, Outcome = cname)
    else {
      dimnames(x) <- list(rname, cname)
      names(dimnames(x)) <- dnn
    }

  tavolo.default(df = x, rev = rev)
}

#' @inheritParams tavolo
#' @export
#' @rdname tavolo
tavolo.table <- function(df, rev = c("neither", "rows", "columns", "both"), ...) {
  x <- df
  rev <- match.arg(rev)

  tavolo.default(df = x, rev = rev)
}
