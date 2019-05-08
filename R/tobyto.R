
#' Create frequency table
#'
#' @param ... argument calls from \code{riskdiff()}
#' @param rev character string indicating whether to switch row or column order, possible options are "neither", "rows", "columns", or "both". The default is "neither".
#'
#' @return
#' @export
#'
#' @examples
tobyto <- function(...) UseMethod("tobyto")

#' @inheritParams tobyto
#' @export
#' @rdname tobyto
tobyto.matrix <- function(..., rev = c("neither", "rows", "columns", "both")) {

  decomp <- list(...)
  x <- decomp[["df"]]
  rev <- match.arg(rev)

  if (ncol(x) > 2) stop("Only binary outcomes allowed")

  rname <- c("Exposed", "Unexposed")
  cname <- c("Outcome", "No Outcome")

  if (is.null(decomp$dnn)) dimnames(x) <- list(Exposure = rname, Outcome = cname)
    else dimnames(x) <- list(rname, cname)
         names(dimnames(x)) <- decomp$dnn

  if (rev == "neither") x <- x
  else if (rev == "rows") x <- x[2:1, ]
  else if (rev == "columns") x <- x[, 2:1]
  else if (rev == "both") x <- x[2:1, 2:1]

  return(x)
}

#' @inheritParams tobyto
#' @export
#' @rdname tobyto
tobyto.table <- function(..., rev = c("neither", "rows", "columns", "both")) {

  decomp <- list(...)
  x <- decomp[["df"]]
  rev <- match.arg(rev)

  if (ncol(x) > 2) stop("Only binary outcomes allowed")

  if (rev == "neither") x <- x
  else if (rev == "rows") x <- x[2:1, ]
  else if (rev == "columns") x <- x[, 2:1]
  else if (rev == "both") x <- x[2:1, 2:1]

  return(x)
}

#' @inheritParams tobyto
#' @export
#' @rdname tobyto
tobyto.data.frame <- function(..., rev = c("neither", "rows", "columns", "both")) {

  decomp <- list(...)
  rev <- match.arg(rev)
  pred <- decomp$x
  outc <- decomp$y

  if (!is.null(decomp$y) && is.null(decomp$weight)) {
    x <- xtabs(~ decomp$df[[pred]] + decomp$df[[outc]])
  } else if (!is.null(decomp$y) && !is.null(decomp$weight)) {
    weight <- decomp$weight
    x <- xtabs(decomp$df[[weight]] ~ decomp$df[[pred]] + decomp$df[[outc]])
  }

  if (is.null(decomp$dnn)) dnn <- c(pred, outc)
    else dnn <- decomp$dnn

  names(dimnames(x)) <- dnn

  if (rev == "neither") x <- x
    else if (rev == "rows") x <- x[2:1, ]
    else if (rev == "columns") x <- x[, 2:1]
    else if (rev == "both") x <- x[2:1, 2:1]

  return(x)
}
