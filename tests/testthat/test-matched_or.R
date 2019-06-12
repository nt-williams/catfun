context("matched_or is working")
library(catfun)

test_that("outputting correct values", {

  set.seed(1)
  gene <- data.frame(pair = seq(1:35),
                     ulcer = rbinom(35, 1, .7),
                     healthy = rbinom(35, 1, .4))

  x <- matched_or(gene, ulcer, healthy)

  expect_equal(x$or$odds_ratio, 0.375)
  expect_equal(round(x$or$lower_bound, 4), 0.1467)
  expect_equal(round(x$or$upper_bound, 4), 0.9583)
})

