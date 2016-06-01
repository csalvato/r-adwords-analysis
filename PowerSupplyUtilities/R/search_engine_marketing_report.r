#' Generate Search Engine Marketing Report
#'
#' Convenience Function that creates a comprehensive Search Engine Marketing report (viz and CSVs) with a single call.
#'
#' @param adwords_keywords_elog A data frame containing keywords and transaction event log data.
#' @export
#' @examples
#' search_engine_marketing_report(adwords_keywords_elog)

search_engine_marketing_report <- function(adwords_keywords_elog){
  require(AdWordsUtilities)
  PowerSupplyUtilities::user_overview(adwords_keywords_elog)
  AdWordsUtilities::keywords_campaign_device_matchtype_report(adwords_keywords_elog)
  AdWordsUtilities::impression_share_over_time(adwords_keywords_elog)
  AdWordsUtilities::click_through_rate_over_time(adwords_keywords_elog)
  AdWordsUtilities::overall_performance_over_time(adwords_keywords_elog, plot = TRUE)
  AdWordsUtilities::contribution_per_click_report(adwords_keywords_elog)
  PowerSupplyUtilities::orders_per_week(adwords_keywords_elog, 
                                                                  keyword_filter="paleo meals", 
                                                                  campaign_filter="Paleo Performers")
  PowerSupplyUtilities::paleo_cohort_views(adwords_keywords_elog)
  PowerSupplyUtilities::customers_per_month_report(adwords_keywords_elog, file="adwords_customers_per_month.csv")
  AdWordsUtilities::mobile_performance_over_time(adwords_keywords_elog, keyword_filter="paleo meals")
  AdWordsUtilities::desktop_performance_over_time(adwords_keywords_elog, keyword_filter="paleo meals")
  AdWordsUtilities::keywords_performance_over_time(adwords_keywords_elog)
  AdWordsUtilities::keywords_campaign_performance_over_time(adwords_keywords_elog)  
  # Should this be made into a report function too?
  # adwords_keywords_elog %>% AdWordsUtilities::summarize_elog()

  require(BingUtilities)
  PowerSupplyUtilities::user_overview(bing_keywords_elog)
  BingUtilities::keywords_campaign_device_matchtype_report(bing_keywords_elog)
  BingUtilities::click_through_rate_over_time(bing_keywords_elog)
  BingUtilities::overall_performance_over_time(bing_keywords_elog, plot = TRUE)
  BingUtilities::contribution_per_click_report(bing_keywords_elog)
  PowerSupplyUtilities::orders_per_week(bing_keywords_elog, keyword_filter="paleo meals")
  PowerSupplyUtilities::paleo_cohort_views(bing_keywords_elog)
  PowerSupplyUtilities::customers_per_month_report(bing_keywords_elog, file="bing_customers_per_month.csv")
  BingUtilities::mobile_performance_over_time(bing_keywords_elog, keyword_filter="paleo meals")
  BingUtilities::desktop_performance_over_time(bing_keywords_elog, keyword_filter="paleo meals")
  BingUtilities::keywords_performance_over_time(bing_keywords_elog)
  BingUtilities::keywords_campaign_performance_over_time(bing_keywords_elog) 
  # Should this be made into a report function too?
  # bing_keywords_elog %>% BingUtilities::summarize_elog()

  #TODO: Move to own function
  total_customers_per_month_report <- merge(adwords_customers_per_month_report, 
                                          bing_customers_per_month_report, 
                                          by="first_transaction_month", 
                                          all=TRUE, 
                                          suffixes=c(".adwords",".bing")) %>% 
                                    mutate_each(funs(ifelse(is.na(.),0,.))) %>% 
                                    mutate(total_acquisitions=num_acquisitions.adwords+num_acquisitions.bing)
  write.excel.csv(total_customers_per_month_report)

}