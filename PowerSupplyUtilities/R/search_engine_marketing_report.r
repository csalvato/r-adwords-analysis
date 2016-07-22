#' Generate Search Engine Marketing Report
#'
#' Convenience Function that creates a comprehensive Search Engine Marketing report (viz and CSVs) with a single call.
#'
#' @param adwords_keywords_elog A data frame containing keywords and transaction event log data from AdWords.
#' @param bing_keywords_elog A data frame containing keywords and transaction event log data from Bing.
#' @param gdn_elog A data frame containing keywords and transaction event log data from Google Display Network.
#' @export
#' @examples
#' search_engine_marketing_report(adwords_keywords_elog)

search_engine_marketing_report <- function( adwords_keywords_elog=NULL, 
                                            bing_keywords_elog=NULL,
                                            gdn_elog=NULL){
  
  if(!is.null(adwords_keywords_elog)) {
    require(AdWordsUtilities)
    PowerSupplyUtilities::user_overview(adwords_keywords_elog)
    AdWordsUtilities::keywords_campaign_device_matchtype_report(adwords_keywords_elog, file="output/adwords_keyword_campaign_device_matchtype_report.csv")
    AdWordsUtilities::impression_share_over_time(adwords_keywords_elog, plot_png_file="output/adwords_impression_share_over_time.png")
    AdWordsUtilities::click_through_rate_over_time(adwords_keywords_elog, plot_png_file="output/adwords_click_through_rate_over_time.png")
    AdWordsUtilities::overall_performance_over_time(adwords_keywords_elog, plot_png_file="output/!adwords_overall_performance_over_time.png")
    AdWordsUtilities::contribution_per_click_report(adwords_keywords_elog, file="output/adwords_contribution_per_click_report.csv")
    PowerSupplyUtilities::orders_per_week(adwords_keywords_elog, 
                                          keyword_filter="paleo meals", 
                                          campaign_filter="Paleo Performers",
                                          plot_png_file="output/@adwords_orders_per_week.png")
    PowerSupplyUtilities::paleo_cohort_views(adwords_keywords_elog)
    adwords_customers_per_month_report <- PowerSupplyUtilities::customers_per_month_report( adwords_keywords_elog, 
                                                                                            file="output/adwords_customers_per_month.csv")
    AdWordsUtilities::mobile_performance_over_time(adwords_keywords_elog, keyword_filter="paleo meals", plot_png_file="output/adwords_mobile_performance_over_time.png")
    AdWordsUtilities::desktop_performance_over_time(adwords_keywords_elog, keyword_filter="paleo meals", plot_png_file="output/adwords_desktop_performance_over_time.png")
    AdWordsUtilities::keywords_performance_over_time(adwords_keywords_elog, plot_png_file="output/adwords_keywords_performance_over_time.png")
    AdWordsUtilities::keywords_campaign_performance_over_time(adwords_keywords_elog, plot_png_file="output/adwords_keywords_campaign_performance_over_time.png")  
    # Should this be made into a report function too?
    # adwords_keywords_elog %>% AdWordsUtilities::summarize_elog()
  }

  if(!is.null(bing_keywords_elog)) {
    require(BingUtilities)
    PowerSupplyUtilities::user_overview(bing_keywords_elog)
    BingUtilities::keywords_campaign_device_matchtype_report(bing_keywords_elog, file="output/bing_keywords_campaign_device_matchtype_report.csv")
    #BingUtilities::click_through_rate_over_time(bing_keywords_elog, plot_png_file="output/bing_click_through_rate_over_time.png")
    BingUtilities::overall_performance_over_time(bing_keywords_elog, plot_png_file="output/!bing_overall_performance_over_time.png")
    BingUtilities::contribution_per_click_report(bing_keywords_elog, file="output/bing_contribution_per_click_report.csv")
    PowerSupplyUtilities::orders_per_week(bing_keywords_elog, keyword_filter="paleo meals", plot_png_file="output/@bing_orders_per_week.png")
    PowerSupplyUtilities::paleo_cohort_views(bing_keywords_elog)
    bing_customers_per_month_report <- PowerSupplyUtilities::customers_per_month_report(bing_keywords_elog, file="output/bing_customers_per_month.csv")
    BingUtilities::mobile_performance_over_time(bing_keywords_elog, keyword_filter="paleo meals", plot_png_file="output/bing_mobile_performance_over_time.png")
    BingUtilities::desktop_performance_over_time(bing_keywords_elog, keyword_filter="paleo meals", plot_png_file="output/bing_desktop_performance_over_time.png")
    BingUtilities::keywords_performance_over_time(bing_keywords_elog, plot_png_file="output/bing_keywords_performance_over_time.png")
    BingUtilities::keywords_campaign_performance_over_time(bing_keywords_elog, plot_png_file="output/bing_keywords_campaign_performance_over_time.png") 
    # Should this be made into a report function too?
    # bing_keywords_elog %>% BingUtilities::summarize_elog()
  }

    if(!is.null(gdn_elog)) {
      require(AdWordsUtilities)
      AdWordsUtilities::overall_performance_over_time(gdn_elog, plot_png_file="output/!gdn_overall_performance_over_time.png")
      AdWordsUtilities::desktop_gdn_performance_over_time(gdn_elog, plot_png_file="output/gdn_desktop_performance_over_time.png")
      AdWordsUtilities::mobile_gdn_performance_over_time(gdn_elog,, plot_png_file="output/gdn_mobile_performance_over_time.png")
      PowerSupplyUtilities::orders_per_week(gdn_elog, 
                                            campaign_filter="Rmktg",
                                            plot_png_file="output/@gdn_orders_per_week.png")

    }

  if( !is.null(adwords_keywords_elog) && !is.null(bing_keywords_elog)) {
    total_customers_per_month_report <- merge(adwords_customers_per_month_report, 
                                            bing_customers_per_month_report, 
                                            by="first_transaction_month", 
                                            all=TRUE, 
                                            suffixes=c(".adwords",".bing")) %>% 
                                      mutate_each(funs(ifelse(is.na(.),0,.))) %>% 
                                      mutate(total_acquisitions=num_acquisitions.adwords+num_acquisitions.bing)
    write.excel.csv(total_customers_per_month_report, file="output/total_customers_per_month_report.csv")
  }
  

}