#' Goodness of fit for logistic regression using deviance
#'
#' Conduct goodness of fit for logistic regression using deviance.
#'
#' @param working a working model of class "glm"
#' @param saturated a saturated model of class "glm"
#'
#' @return a data frame of class "tibble" with the following columns:
#'
#' \item{model}{the corresponding model}
#' \item{deviance}{the deviance for corresponding model}
#' \item{df.residual}{the residual degrees of freedom for corresponding model}
#' \item{stat}{the test statistic, follows a chi-squared distribution}
#' \item{df}{degrees of freedom for test statistic}
#' \item{p.value}{p-value corresponding to test statistic and degrees of freedom}
#'
#' @examples
#'
#' cad <- data.frame(gender = c(rep("male", 6), rep("female", 6)),
#'                   agecat = c(rep(c("<60", "60-69", "70+"), 4)),
#'                   event = c(rep(c(0, 0, 0, 1, 1, 1), 2)),
#'                   count = c(110, 131, 126, 13, 37, 45, 411, 110, 189, 41, 24, 12))
#'
#' saturated <- glm(event ~ gender*agecat, weight = count, data = cad, family = "binomial")
#' working <- glm(event ~ gender + agecat, weight = count, data = cad, family = "binomial")
#' gof_deviance(working, saturated)
#'
#' @export
gof_deviance <- function(working, saturated) {
  work <- broom::glance(working) %>%
    dplyr::select(deviance, df.residual)
  sat <- broom::glance(saturated) %>%
    dplyr::select(deviance, df.residual)

  dplyr::bind_rows(work, sat) %>%
    dplyr::mutate(model = c("working", "saturated"),
           stat = deviance - dplyr::lead(deviance),
           df = df.residual - dplyr::lead(df.residual),
           p.value = stats::pchisq(stat, df, lower.tail = FALSE)) %>%
    dplyr::select(model, dplyr::everything())
}
