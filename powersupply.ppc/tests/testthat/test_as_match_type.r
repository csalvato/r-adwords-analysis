library(powersupply.ppc)
context("Match Type")

test_that("as.match_type converts b to 'Broad'", {
  expect_equal(as.match_type(c("b","b")), c("Broad", "Broad"))
  expect_equal(as.match_type(c("e","e")), c("Exact", "Exact"))
  expect_equal(as.match_type(c("p","p")), c("Phrase", "Phrase"))
  expect_equal(as.match_type(c("p","e","b")), c("Phrase", "Exact", "Broad"))
})
