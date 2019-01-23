
#' Power and sample size for 2 proportions
#'
#' Calculate power and sample size for comparison of 2 proportions for both balanced and ubalanced designs.
#'
#' @importFrom Hmisc bsamsize bpower
#' @importFrom stats power.prop.test
#' @export

prop_power <- function(n, n1, n2, p1, p2,
                       fraction = 0.5, alpha = 0.05, power = NULL,
                       alternative = c("two.sided", "one.sided"), strict = FALSE,
                       tol = .Machine$double.eps^0.25, odds.ratio, percent.reduction) {

  if (fraction == 0.5) {
    if (!missing(n1) | !missing(n2)) {
      stop("If using a balanced design, n1 and n2 must be left blank")
    }

    if (!missing(odds.ratio)) {
      if (!missing(p2)) {
        stop("Either manually enter p2 or use odds ratio, not both")
      } else {
        p2 <- p1 * odds.ratio/(1 - p1 + p1 * odds.ratio)
      }
      p2 <- p1 * odds.ratio/(1 - p1 + p1 * odds.ratio)
    } else if (!missing(percent.reduction)) {
      if (!missing(p2)) {
        stop("Either manually enter p2 or use percent reduction, not both")
      } else {
        p2 <- p1 * (1 - percent.reduction/100)
      }
    }

    if (missing(power)) {
      base <- power.prop.test(n = (n/2), p1 = p1, p2 = p2, sig.level = alpha, power = NULL,
                              alternative = alternative, strict = strict, tol = tol)

    } else if (missing(n)) {
      base <- power.prop.test(n = NULL, p1 = p1, p2 = p2, sig.level = alpha, power = power,
                              alternative = alternative, strict = strict, tol = tol)
    }

    n <- (base$n)*2
    n1 <- base$n
    n2 <- base$n
    p1 <- base$p1
    p2 <- base$p2
    power <- base$power
    sig.level <- base$sig.level

  } else {

    if (missing(power)) {

      hm <- Hmisc::bpower(p1 = p1, p2 = p2, n = n, n1 = n1, n2 = n2, alpha = alpha,
                   odds.ratio = odds.ratio, percent.reduction = percent.reduction)

      base <- power.prop.test(n = (n/2), p1 = p1, p2 = p2, sig.level = alpha, power = power,
                              alternative = alternative, strict = strict, tol = tol)

      n <- (base$n)*2
      n1 <- n1
      n2 <- n2
      p1 <- base$p1
      p2 <- base$p2
      power <- hm
      sig.level <- base$sig.level


    } else {
      if (!missing(n) | !missing(n1) | !missing(n2)) {
        stop("If given power, n, n1, and n2 must be left blank")
      } else {
        hm <- Hmisc::bsamsize(p1 = p1, p2 = p2, fraction = fraction, alpha = alpha, power = power)

        base <- power.prop.test(n = NULL, p1 = p1, p2 = p2, sig.level = alpha, power = power,
                                alternative = alternative, strict = strict, tol = tol)

        n <- sum(hm[1], hm[2])
        n1 <- hm[1]
        n2 <- hm[2]
        p1 <- base$p1
        p2 <- base$p2
        power <- base$power
        sig.level <- base$sig.level

      }
    }
  }

  out <- list(n = n, n1 = n1, n2 = n2, power = power, p1 = p1, p2 = p2, sig.level = sig.level)
  class(out) <- "prop_power"
  print(out)

}


