library(powersupply.ppc)
context("As Device")

test_that("as.device converts 'Computers' to 'dt'", {
  expect_equal(as.device(c("Computers","Computers")), c("dt", "dt"))
})

test_that("as.device converts 'Tablets with full browsers' to 'dt'", {
  expect_equal(as.device(c("Tablets with full browsers","Tablets with full browsers")), c("dt", "dt"))
})

test_that("as.device converts 'Mobile devices with full browsers' to 'mb'", {
  expect_equal(as.device(c("Mobile devices with full browsers","Mobile devices with full browsers")), c("mb", "mb"))
})

test_that("as.device converts a vector with multiple values properly", {
  expect_equal(as.device(c("Computers","Tablets with full browsers", "Mobile devices with full browsers")), c("dt", "dt","mb"))
})
