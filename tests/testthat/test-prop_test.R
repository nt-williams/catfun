
context("prop_test is working")
library(catfun)

test_that("prop_test agrees with prop.test/BinomCI/SAS for single proportion", {
  pt_stat <- prop_test(2, 6)
  pT_stat <- prop.test(2, 6, correct = FALSE)
  binom <- DescTools::BinomCI(2, 6, method = "wald")
  b_exact <- binom.test(2, 6)
  attr(b_exact$conf.int, "conf.level") <- NULL

  expect_equal(pt_stat$statistic, pT_stat$statistic)
  expect_equal(round(pt_stat$p_value, 4), 0.4142)
  expect_equal(pt_stat$method_ci, binom[, 2:3])
  expect_equal(pt_stat$exact_ci, b_exact$conf.int)
})
