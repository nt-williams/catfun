
context("riskdiff is working")
library(catfun)
library(dplyr)
library(forcats)

test_that("riskdiff agrees with SAS", {
  trial <- tibble(
    disease = c(rep("yes", 2), rep("no", 2)),
    treatment = c(rep(c("estrogen", "placebo"), 2)),
    count = c(751, 623, 7755, 7479)
  )

  trial <- trial %>%
    mutate(disease = as_factor(disease),
           treatment = as_factor(treatment))

  risk_test <- trial %>% riskdiff("treatment", "disease", weight = "count")

  expect_equal(round(risk_test$ci, 4), c(0.0030, 0.0198))
})
