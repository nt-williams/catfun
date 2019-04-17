
context("prop_test is working")
library(catfun)

test_that("prop_test agrees with prop.test/BinomCI/SAS for single proportion", {
  pt_stat <- prop_test(3, 45)
  pT_stat <- prop.test(3, 45, correct = FALSE)
  binom <- DescTools::BinomCI(3, 45, method = "wald")
  b_exact <- binom.test(3, 45)
  attr(b_exact$conf.int, "conf.level") <- NULL

  expect_equal(pt_stat$statistic, pT_stat$statistic)
  expect_equal(round(pt_stat$p_value, 4), round(pchisq(33.8, df = 1, lower.tail = FALSE)))
  expect_equal(pt_stat$method_ci, binom[, 2:3])
  expect_equal(pt_stat$exact_ci, b_exact$conf.int)
})
