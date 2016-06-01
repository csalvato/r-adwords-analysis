#' Generate Search Engine Marketing Report
#'
#' Convenience Function that creates a comprehensive Search Engine Marketing report (viz and CSVs) with a single call.
#'
#' @param adwords_keywords_elog A data frame containing keywords and transaction event log data.
#' @export
#' @examples
#' search_engine_marketing_report(adwords_keywords_elog)

search_engine_marketing_report <- function(adwords_keywords_elog){
  adwords_user_overview <- user_overview(adwords_keywords_elog)
  AdWordsUtilities::keywords_campaign_device_matchtype_report(adwords_keywords_elog)
  keywords_weekly_conversion_metrics <- impression_share_over_time(adwords_keywords_elog)
  keywords_weekly_conversion_metrics <- click_through_rate_over_time(adwords_keywords_elog)
  adwords_overall_performance_over_time <- AdWordsUtilities::overall_performance_over_time(adwords_keywords_elog, plot = TRUE)
  AdWordsUtilities::contribution_per_click_report(adwords_keywords_elog)
  adwords_order_per_week <- PowerSupplyUtilities::orders_per_week(adwords_keywords_elog, 
                                                                  keyword_filter="paleo meals", 
                                                                  campaign_filter="Paleo Performers")
  paleo_cohort_views(adwords_keywords_elog)
  adwords_customers_per_month_report <- PowerSupplyUtilities::customers_per_month_report(adwords_keywords_elog, file="adwords_customers_per_month.csv")
  devices_over_time <- mobile_performance_over_time(adwords_keywords_elog, keyword_filter="paleo meals")
  devices_over_time <- desktop_performance_over_time(adwords_keywords_elog, keyword_filter="paleo meals")
  keywords_with_earnings <- keywords_with_earnings(adwords_keywords_elog)
  keywords_over_time <- keywords_performance_over_time(adwords_keywords_elog)
  keywords_campaigns_over_time <- keywords_campaign_performance_over_time(adwords_keywords_elog)
  adwords_summary_overview <- adwords_keywords_elog %>%
                              AdWordsUtilities::summarize_elog()

}