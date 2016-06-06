#' Create Keywords Event Log (elog) for Bing
#'
#' Created an event log (elog) of all transactions, referrals and keyword activity.  An event log
#' is simple a time-series log of events that happen.  When a certain row (observation) does not include
#' a particular variable, that variable is marked as NA.  For example, a Bing keyword observation will
#' have information about the number of clicks, but no revenue for the transaction.  Revenue data, instead
#' is pulled in from the order application's data.
#'
#' @param from Start date in either format <"yyyy-mm-dd"> or <yyyymmdd>. Inclusive.
#' @param to End date in either format <"yyyy-mm-dd"> or <yyyymmdd>. Inclusive.
#' @return A data frame with all events (Bing clicks, Transactions and Referrals).
#' @export
#' @examples
#' create_bing_event_log(from=20150101, to=20151231)

create_bing_event_log <- function(from=Sys.Date(), 
                                  to=Sys.Date()){
  require(BingUtilities)
  require(SalvatoUtilities)

  ppc_events <- all_ppc_raw_completed_order_events( from = from, to = to )
  mixpanel_bing_conversions <- ppc_events[["bing"]]
  mixpanel_bing_conversions <- clean_completed_order_events(mixpanel_bing_conversions)
  #Clean up keywords info for future lookup of Keyword name with Keyword ID from Bing data.
  mixpanel_bing_conversions <- mixpanel_bing_conversions %>% 
                               mutate(keyword_id = as.numeric(gsub("kwd-","",latest_ad_awkeyword))) %>%
                               select(-latest_ad_awkeyword)

  ##### Retrieve Bing Spend/Click Data
  bing_keywords_data <- BingUtilities::keyword_performance_data(from=as.Date(from), to=as.Date(to))

  mixpanel_bing_conversions <- bing_keywords_data %>% 
                                group_by(keyword_id)%>% 
                                summarize(keyword = first(keyword)) %>%
                                right_join(mixpanel_bing_conversions, by=c(keyword_id = "keyword_id"))

  # Retrieve revenue data
  db_transactions <- get_transactions_data(from=from, to=to)
  db_influencer_metrics <- get_referrals_data(from=from, to=to)
  db_first_transactions  <- get_first_transaction_dates_for_users(from=from, to=to, users=db_transactions$app_user_id)
  
  # Join Mixpanel Conversion Data with transaction data
  unique_users <- distinct(mixpanel_bing_conversions, app_user_id)
  db_transactions  <- db_transactions %>% inner_join(unique_users, by="app_user_id")

  #Filter out people where their first order was not in the specified start date and end date
  db_transactions <- db_transactions %>% filter(is.element(app_user_id, db_first_transactions$id))

  # Format database transactions for future use
  db_transactions <- clean_transactions_data_for_bing(db_transactions)

  # Add campaign names to db_transactions log
  db_transactions <- bing_keywords_data %>% 
                       group_by(campaign_id)%>% 
                       summarize(campaign_name = first(campaign_name)) %>%
                       right_join(db_transactions, by=c(campaign_id = "campaign_id"))


  ###################################### CREATE ELOGS ################################################
  # Create keywords elog
  keywords_elog <- rbind.fill(db_transactions, bing_keywords_data)
  keywords_elog$week <- as.week(keywords_elog$date)
  keywords_elog <- keywords_elog %>% arrange(week)

  # Create join table for user_id and the keyword and campaign_name of first purchase
  user_first_acquisition_metrics <- keywords_elog %>%
                                    filter(!is.na(user_id)) %>% #Remove NA user_ids (which means they are not monetary transactions)
                                    group_by(user_id) %>%
                                    summarize(keyword = first(keyword),
                                              campaign_name=first(campaign_name),
                                              campaign_id=first(campaign_id),
                                              device=first(device),
                                              match_type=first(match_type))

  #Add influencer metrics to the event log
  influencer_metrics_with_user_data <- db_influencer_metrics %>%
                                      rename(week=week_start,
                                             user_id=influencer_id) %>%
                                      mutate(week = as.Date(week, format = '%Y-%m-%d')) %>%
                                      inner_join(user_first_acquisition_metrics, by=c(user_id="user_id")) %>% 
                                      filter(week >= from, week <= to)

  keywords_elog <- rbind.fill(keywords_elog, influencer_metrics_with_user_data)

  keywords_elog <- keywords_elog %>% 
                    select( -transaction_date,
                            -event,
                            -time,
                            -distinct_id,
                            -X.created,
                            -X.email,
                            -X.first_name,
                            -X.last_name,
                            -X.lib_version,
                            -X.name,
                            -currency,
                            -discount,
                            -mp_country_code,
                            -mp_lib,
                            -products,
                            -tax,
                            -total,
                            -lastOrderedFrom,
                            -utm_content,
                            -mp_keyword,
                            -traits,
                            -app_user_id)

  return(keywords_elog)
}