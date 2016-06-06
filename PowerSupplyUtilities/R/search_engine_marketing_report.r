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
  adwords_customers_per_month_report <- PowerSupplyUtilities::customers_per_month_report(adwords_keywords_elog, file="adwords_customers_per_month.csv")
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
  bing_customers_per_month_report <- PowerSupplyUtilities::customers_per_month_report(bing_keywords_elog, file="bing_customers_per_month.csv")
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

  #Hacky Solution to get GDN Performance Graphs
  gdn_elog <- AdWordsUtilities::create_google_display_event_log(from, to)
  gdn_elog <- gdn_elog %>% 
                filter(grepl("Rmktg",campaign_name)) %>% 
                mutate(quality_score=NA, 
                       new_referred_users=NA, 
                       referred_users_transaction_amount=NA)

  AdWordsUtilities::overall_performance_over_time(gdn_elog)
  
  devices_over_time <- gdn_elog %>%
                      group_by(device,week) %>%
                      summarize(cost = sum(cost, na.rm = TRUE),
                                contribution = sum(money_in_the_bank_paid_to_us,na.rm=TRUE) *.25) %>%
                      mutate(cum_contribution = cumsum(contribution), 
                             cum_cost = cumsum(cost),
                             cum_ROI = cum_contribution - cum_cost) %>%
                      gather(type,value,cum_cost,cum_contribution,cum_ROI)

  plot(ggplot(devices_over_time %>% 
              filter(device == "mb"), 
          aes(week,value,group=type,col=type,fill=type)) + 
           geom_line() + 
           ggtitle("GDN Trends on Mobile"))

  plot(ggplot(devices_over_time %>% 
              filter(device == "dt"), 
          aes(week,value,group=type,col=type,fill=type)) + 
           geom_line() + 
           ggtitle("GDN Trends on Desktop"))
  

}