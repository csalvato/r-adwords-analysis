library(SalvatoUtilities)

context("Date Filter")

test_that("Date Filter removes all dates outside of start_date and end_date", {
  data <- data.frame(date = as.Date(c("2015-12-16", "2016-02-01", "2017-03-01")), 
	  								 a = c("1","2","3"), 
										 b = c("3","2","1"))
	filtered_data <- date_filter(data, as.Date("2016-01-01"), as.Date("2016-02-02"))
  expected_results <- data.frame(date = as.Date("2016-02-01"),
  															 a = "2",
  															 b = "2")
  expect_that(filtered_data, is_equivalent_to(expected_results))
})

test_that("Date Filter removes all dates outside of today without arguments", {
  data <- data.frame(date = as.Date(c("2015-12-16", format(Sys.Date(), "%Y-%m-%d"), "2017-03-01")), 
	  								 a = c("1","2","3"), 
										 b = c("3","2","1"))

	filtered_data <- date_filter(data)
  expected_results <- data.frame(date = Sys.Date(),
  															 a = "2",
  															 b = "2")
  expect_that(filtered_data, is_equivalent_to(expected_results))
})