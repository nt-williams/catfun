
tobyto <- function(...) UseMethod("tobyto")

tobyto.matrix <- function(..., rev = c("neither", "rows", "columns", "both")) {

  # NEEDS IMPLEMENTATION OF ERROR CHECK

  decomp <- list(...)
  x <- decomp[[1]]
  rev <- match.arg(rev)

  if (ncol(x) > 2) stop("Only binary outcomes allowed")

  rname <- c("Exposed", "Unexposed")
  cname <- c("Outcome", "No Outcome")

  dimnames(x) <- list(Exposure = rname, Outcome = cname)

  if (rev == "neither") x <- x
  else if (rev == "rows") x <- x[2:1, ]
  else if (rev == "columns") x <- x[, 2:1]
  else if (rev == "both") x <- x[2:1, 2:1]

  return(x)
}

tobyto.data.frame <- function(..., rev = c("neither", "rows", "columns", "both")) {

  # NEEDS IMPLEMENTATION OF ERROR CHECK

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

  names(dimnames(x)) <- c("Exposure", "Outcome")

  if (rev == "neither") x <- x
    else if (rev == "rows") x <- x[2:1, ]
    else if (rev == "columns") x <- x[, 2:1]
    else if (rev == "both") x <- x[2:1, 2:1]

  return(x)
}
