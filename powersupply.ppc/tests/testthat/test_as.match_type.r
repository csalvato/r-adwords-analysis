library(powersupply.ppc)
context("As Match Type")

test_that("as.match_type converts 'b' to 'Broad'", {
  expect_equal(as.match_type(c("b","b")), c("Broad", "Broad"))
})

test_that("as.match_type converts 'e' to 'Exact'", {
  expect_equal(as.match_type(c("e","e")), c("Exact", "Exact"))
})

test_that("as.match_type converts 'p' to 'Phrase'", {
  expect_equal(as.match_type(c("p","p")), c("Phrase", "Phrase"))
})

test_that("as.match_type converts a vector with multiple values properly", {
  expect_equal(as.match_type(c("p","e","b")), c("Phrase", "Exact", "Broad"))
})
