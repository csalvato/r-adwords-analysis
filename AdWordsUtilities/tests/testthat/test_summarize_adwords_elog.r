library(powersupply.ppc)

context("Summarize a full AdWords Event Log")
test_adwords_elog <- readRDS("test_adwords_elog.rds")
calculated_results <- summarize_adwords_elog(test_adwords_elog)

test_that("summarize_adwords_elog cost column is the total of all values in cost column", {
  expected_cost <- 35.67
  expect_equal(calculated_results$cost, expected_cost)
})

test_that("summarize_adwords_elog average_position column is the weighted mean of the average position (within 2 significant figures)", {
  expected_average_position <- signif(4.118865,2)
  expect_equal(signif(calculated_results$average_position,2), expected_average_position)
})

test_that("summarize_adwords_elog average_quality_score column is the average of all values in the quality_score column", {
  expected_average_quality_score <- 5
  expect_equal(calculated_results$average_quality_score, expected_average_quality_score)
})

test_that("summarize_adwords_elog estimated_available_impressions column is the sum of impressions over estimated impression share for each row", {
  expected_estimated_available_impressions <- signif(1945.085,3)
  expect_equal(signif(calculated_results$estimated_available_impressions,3), expected_estimated_available_impressions)
})
test_that("summarize_adwords_elog impressions column is the sum of all impressions", {
  expected_impressions <- 705
  expect_equal(calculated_results$impressions, expected_impressions)
})
test_that("summarize_adwords_elog est_search_impression_share column is total impressions over estimated available impressions", {
  expected_est_search_impression_share <- signif(0.362452,3)
  expect_equal(signif(calculated_results$est_search_impression_share,3), expected_est_search_impression_share)
})
test_that("summarize_adwords_elog clicks column is the sum of all clicks", {
  expected_clicks <- 11
  expect_equal(calculated_results$clicks, expected_clicks)
})
test_that("summarize_adwords_elog click_through_rate column is sum of all clicks over sum of all impressions", {
  expected_click_through_rate <- 0.01560284
  expect_equal(calculated_results$click_through_rate, expected_click_through_rate)
})
test_that("summarize_adwords_elog num_acquisitions column is the count of distinct users purchasing in that time frame for the first time", {
  skip("This function not designed properly.
        Number of acqusitions should tell the number of 
        people who were acquired with a first purchase 
        within the time frame to calculate CTR properly.")
  expected_num_acquisitions <- 14
  expect_equal(calculated_results$num_acquisitions, expected_num_acquisitions)
})
test_that("summarize_adwords_elog conversion_rate column is the amount of people purchasing over the amount of clicks.", {
  expected_conversion_rate <- signif(1.272727,3)
  expect_equal(signif(calculated_results$conversion_rate,3), expected_conversion_rate)
})
test_that("summarize_adwords_elog cost_per_click column is the sum of cost over the sum of clicks", {
  expected_cost_per_click <- signif(3.242727,3)
  expect_equal(signif(calculated_results$cost_per_click,3), expected_cost_per_click)
})
test_that("summarize_adwords_elog earnings column is the sum of all money paid to us", {
  expected_earnings <- 1515.83
  expect_equal(calculated_results$earnings, expected_earnings)
})
test_that("summarize_adwords_elog contribution column is earnings multiplied by margin", {
  expected_contribution <- signif(378.9575,3)
  expect_equal(signif(calculated_results$contribution,3), expected_contribution)
})
test_that("summarize_adwords_elog earnings_per_click column is sum of earnigns over the sum of clicks", {
  expected_earnings_per_click <- signif(137.8027,3)
  expect_equal(signif(calculated_results$earnings_per_click,3), expected_earnings_per_click)
})
test_that("summarize_adwords_elog contribution_per_click column is the sum of contribution over the sum of clicks.", {
  expected_contribution_per_click <- signif(34.45068,3)
  expect_equal(signif(calculated_results$contribution_per_click,3), expected_contribution_per_click)
})
test_that("summarize_adwords_elog cpa column is the cost of contribution over the cost of clicks.", {
  expected_cpa <- signif(2.547857,3)
  expect_equal(signif(calculated_results$cpa,3), expected_cpa)
})
test_that("summarize_adwords_elog referred_users column is the sum of all referred users", {
  expected_referred_users <- 0
  expect_equal(calculated_results$referred_users, expected_referred_users)
})
test_that("summarize_adwords_elog referred_earnings column is sum of all earnings by referees of referers", {
  expected_referred_earnings <- 0
  expect_equal(calculated_results$referred_earnings, expected_referred_earnings)
})
test_that("summarize_adwords_elog estimated_ltv column is number of acquisitions multipled estimated spend", {
  expected_estimated_ltv <- 2450
  expect_equal(calculated_results$estimated_ltv, expected_estimated_ltv)
})
test_that("summarize_adwords_elog estimated_lifetime_ROAS column is estimated spend over cost", {
  expected_estimated_lifetime_ROAS <- 67.68517
  expect_equal(calculated_results$estimated_lifetime_ROAS, expected_estimated_lifetime_ROAS)
})
test_that("summarize_adwords_elog total_earnings  column is the sum of referred and unreferred earnings", {
  expected_total_earnings  <- 1515.83
  expect_equal(calculated_results$total_earnings , expected_total_earnings )
})
test_that("summarize_adwords_elog total_contribution  column is the sum of referred and unreferred contribution", {
  expected_total_contribution  <- 378.9575
  expect_equal(calculated_results$total_contribution , expected_total_contribution )
})
test_that("summarize_adwords_elog actual_ROAS  column is the return on ad spend (total_contribution - cost)/cost", {
  expected_actual_ROAS  <- signif(9.623984,3)
  expect_equal(signif(calculated_results$actual_ROAS,3), expected_actual_ROAS )
})