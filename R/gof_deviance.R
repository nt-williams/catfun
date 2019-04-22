#' Conduct goodness of fit for logistic regression using deviance
#'
#' @param working
#' @param saturated
#'
#' @return
#' @export
#'
#' @examples
gof_deviance <- function(working, saturated) {
  work <- broom::glance(working) %>%
    select(deviance, df.residual)
  sat <- broom::glance(saturated) %>%
    select(deviance, df.residual)

  bind_rows(work, sat) %>%
    mutate(model = c("working", "saturated"),
           stat = deviance - lead(deviance),
           df = df.residual - lead(df.residual),
           p.value = pchisq(stat, df, lower.tail = FALSE)) %>%
    select(model, everything())
}
