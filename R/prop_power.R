
#' Power and sample size for 2 proportions
#'
#' Calculate power and sample size for comparison of 2 proportions for both balanced and ubalanced designs.
#'
#' @param n total sample size
#' @param n1 sample size in group 1
#' @param n2 sample size in group 2
#' @param p1 group 1 proportion
#' @param p2 group 2 proportion
#' @param fraction fraction of total observations that are in group 1
#' @param alpha signficance level/type 1 error rate
#' @param power desired power, between 0 and 1
#' @param alternative alternative hypothesis, one- or two-sided test
#' @param odds.ratio odds ratio comparing p2 to p2
#' @param percent.reduction percent reduction of p1 to p2
#'
#' @examples
#' prop_power(n = 220, p1 = 0.35, p2 = 0.2)
#' prop_power(p1 = 0.35, p2 = 0.2, fraction = 2/3, power = 0.85)
#' prop_power(p1 = 0.35, n = 220, percent.reduction = 42.857)
#' prop_power(p1 = 0.35, n = 220, odds.ratio = 0.4642857)
#'
#' @return a list with class "prop_power" containing the following components:
#'
#' \item{n}{the total sample size}
#' \item{n1}{the sample size in group 1}
#' \item{n2}{the sample size in group 2}
#' \item{p1}{the proportion in group 1}
#' \item{p2}{the proportion in group 2}
#' \item{power}{calculated or desired power}
#' \item{sig.level}{level of signficance}
#'
#' @importFrom Hmisc bsamsize bpower
#' @importFrom stats power.prop.test
#' @export

prop_power <- function(n, n1, n2, p1, p2, fraction = 0.5, alpha = 0.05, power = NULL,
                       alternative = c("two.sided", "one.sided"), odds.ratio, percent.reduction) {

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
                              alternative = alternative)

    } else if (missing(n)) {
      base <- power.prop.test(n = NULL, p1 = p1, p2 = p2, sig.level = alpha, power = power,
                              alternative = alternative)
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
                              alternative = alternative)

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
                                alternative = alternative)

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


