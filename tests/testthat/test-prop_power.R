
context("prop_power is working")
library(catfun)

test_that("outputting correct values", {
  x <- prop_power(n = 220, p1 = 0.35, p2 = 0.2)
  y <- prop_power(p1 = 0.35, p2 = 0.2, fraction = 2/3, power = 0.85)
  names(y$n2) <- NULL

  expect_equal(round(x$power, 4), 0.7051)
  expect_equal(round(y$n2), 120)
})

test_that("errors are occuring correctly", {
  expect_error(prop_power(n = 220, n1 = 110, p1 = 0.35, p2 = 0.2),
               "If using a balanced design, n1 and n2 must be left blank")
  expect_error(prop_power(p1 = 0.35, p2 = 0.2, n = 220, odds.ratio = 0.4642857),
               "Either manually enter p2 or use odds ratio, not both")
  expect_error(prop_power(p1 = 0.35, p2 = 0.2, n = 220, percent.reduction = 42.857),
               "Either manually enter p2 or use percent reduction, not both")
  expect_error(prop_power(n = 220, p1 = 0.35, p2 = 0.2, fraction = 2/3, power = 0.85),
               "If given power, n, n1, and n2 must be left blank")
})
