#' Create 2 x k frequency tables
#'
#' Helper function for creating 2 x k frequency tables.
#'
#' @param df a dataframe with binary variable y and categorical variable x or a 2 x k frequency table/matrix. If a table or matrix, x and y must be NULL. Used to select method.
#' @param ... further arguments passed to or from other methods.
#'
#' @return
#' \item{tab}{2 x k frequency table}
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

#' @export
tavolo.default <- function(df, ...) {
  x <- df
  rev <- list(...)

  if (rev["rev"] == "neither") x <- x
  else if (rev["rev"] == "rows") x <- x[2L:1L, ]
  else if (rev["rev"] == "columns") x <- x[, 2L:1L]
  else if (rev["rev"] == "both") x <- x[2L:1L, 2L:1L]

  return(x)
}

#' Create 2 x k frequency tables
#'
#' Helper function for creating 2 x k frequency tables.
#'
#' @param df a dataframe with binary variable y and categorical variable x.
#' @param x categorical predictor/exposure, vector.
#' @param y binary outcome, vector.
#' @param weight an optional vector of count weights.
#' @param rev character string indicating whether to switch row or column order, possible options are "neither", "rows", "columns", or "both". The default is "neither".
#' @param ... further arguments passed to or from other methods.
#'
#' @return
#' \item{tab}{2 x k frequency table}
#' @export
#'
#' @examples
#' trial <- data.frame(disease = c(rep("yes", 2), rep("no", 2)),
#'                     treatment = c(rep(c("estrogen", "placebo"), 2)),
#'                     count = c(751, 623, 7755, 7479))
#'
#' tavolo(trial, treatment, disease, count)
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

#' Create 2 x k frequency tables
#'
#' Helper function for creating 2 x k frequency tables.
#'
#' @param df a 2 x k frequency matrix.
#' @param dnn optional character vector of dimension names.
#' @param rev character string indicating whether to switch row or column order, possible options are "neither", "rows", "columns", or "both". The default is "neither".
#' @param ... further arguments passed to or from other methods.
#'
#' @return
#' \item{tab}{2 x k frequency table}
#' @export
#'
#' @examples
#' tavolo.matrix(matrix(c(23, 45, 67, 12), nrow = 2, ncol = 2), rev = "both")
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

#' Create 2 x k frequency tables
#'
#' Helper function for creating 2 x k frequency tables.
#'
#' @param df a 2 x k frequency table.
#' @param rev character string indicating whether to switch row or column order, possible options are "neither", "rows", "columns", or "both". The default is "neither".
#' @param ... further arguments passed to or from other methods.
#'
#' @return
#' \item{tab}{2 x k frequency table}
#' @export
#'
#' @examples
#' trial <- data.frame(disease = c(rep("yes", 3), rep("no", 3)),
#'                     treatment = rep(c("estrogen", "placebo", "other"), 2),
#'                     count = c(751, 623, 7755, 7479, 9000, 456))
#'
#' xtabs(count ~ treatment + disease, data = trial) %>% tavolo.table(rev = "columns")
tavolo.table <- function(df, rev = c("neither", "rows", "columns", "both"), ...) {
  x <- df
  rev <- match.arg(rev)

  tavolo.default(df = x, rev = rev)
}
