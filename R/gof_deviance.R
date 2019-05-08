#' Conduct goodness of fit for logistic regression using deviance
#'
#' @param working a working model of class "glm"
#' @param saturated a saturated model of class "glm"
#'
#' @return
#' @export
#'
#' @examples
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
