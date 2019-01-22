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
  }







  else {

    if (is.null(power)) {

      hm_power <- bpower(p1 = p1, p2 = p2, n = n, alpha = alpha, odds.ratio = odds.ratio,
                         percent.reduction = percent.reduction)
    }

    base <- power.prop.test(n = (n/2), p1 = p1, p2 = p2, sig.level = alpha, power = power,
                            alternative = alternative, strict = strict, tol = tol)

    non_base <- Hmisc::bsamsize(p1 = p1, p2 = p2, fraction = fraction, alpha = alpha, power = power)
  }

  print(base)

}

prop_power(n = 220, p1 = 0.35, p2 = 0.2)
prop_power(n1 = 110, n2 = 110, p1 = 0.35, p2 = 0.2)

power.prop.test(n = 110,
                p1 = 0.35, p2 = 0.2,
                sig.level = 0.05)

bpower(0.35, 0.2, n = 220)
bpower(p1 = 0.35, n = 220, percent.reduction = 42.857)
bpower(p1 = 0.2, n = 220, odds.ratio = 2.153846)
