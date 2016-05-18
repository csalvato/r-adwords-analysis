#' Create Keywords Event Log (elog)
#'
#' Created an event log (elog) of all transactions, referrals and keyword activity.  An event log
#' is simple a time-series log of events that happen.  When a certain row (observation) does not include
#' a particular variable, that variable is marked as NA.  For example, an AdWords keyword observation will
#' have information about the number of clicks, but no revenue for the transaction.  Revenue data, instead
#' is pulled in from the order application's data.
#'
#' @param from Start date in either format <"yyyy-mm-dd"> or <yyyymmdd>. Inclusive.
#' @param to End date in either format <"yyyy-mm-dd"> or <yyyymmdd>. Inclusive.
#' @return A data frame with all events (AdWords clicks, Transactions and Referrals).
#' @export
#' @examples
#' create_event_log(from=20150101, to=20151231)

create_event_log <- function(from=Sys.Date(), 
                                  to=Sys.Date()){
  
  ppc_events <- all_ppc_raw_completed_order_events( from = start_date, to = end_date )
  mixpanel_adwords_conversions <- ppc_events[["adwords"]]
  mixpanel_adwords_conversions <- clean_adwords_raw_completed_order_events(mixpanel_adwords_conversions)
  mixpanel_bing_conversions <- ppc_events[["bing"]]

  ##### Retrieve AdWords Spend/Click Data
  adwords_keywords_data <- keyword_performance_data(from=as.Date(start_date), to=as.Date(end_date))
  adwords_campaigns_data <- campaign_performance_data(from=as.Date(start_date), to=as.Date(end_date))

  # Retrieve revenue data
  db_transactions <- get_transactions_data(from=start_date, to=end_date)
  db_influencer_metrics <- get_referrals_data(from=start_date, to=end_date)

  pgsql <- JDBC("org.postgresql.Driver", "database_drivers/postgresql-9.4.1208.jre6.jar", "`")
  heroku_db <- dbConnect(pgsql, string_from_file("jdbc_heroku_string.txt"))
  db_first_transactions <- dbGetQuery(heroku_db, GetoptLong::qq(paste("SELECT 
                                                                          * 
                                                                       FROM
                                                                         (select 
                                                                         min(t.created_at) as first_transaction,
                                                                         u.id,
                                                                         u.name
                                                                         from users u
                                                                         inner join
                                                                         transactions t on t.user_id = u.id
                                                                         where
                                                                         u.id IN (", paste(shQuote(db_transactions$app_user_id, type = "sh"), collapse=','),
                                                                                  ")
                                                                         group by u.id
                                                                         ORDER BY first_transaction desc) first_transactions
                                                                       WHERE
                                                                       first_transaction between '@{start_date}' and '@{end_date}'")))
  heroku_db <- dbDisconnect(heroku_db)

  # Join Mixpanel Conversion Data with transaction data
  unique_users <- distinct(mixpanel_adwords_conversions, app_user_id)

  db_transactions  <- db_transactions %>% inner_join(unique_users, by="app_user_id")

  #Filter out people where their first order was not in the specified start_date and end_date
  db_transactions <- db_transactions %>% filter(is.element(app_user_id, db_first_transactions$id))

  # Format database transactions for future use
  db_transactions <- db_transactions %>% rename(device=latest_ad_device, 
                                                campaign_id=latest_ad_utm_campaign,
                                                keyword=latest_ad_awkeyword,
                                                ad_group_id=latest_ad_awadgroupid,
                                                match_type=latest_ad_awmatchtype)
  db_transactions$campaign_id <- as.integer(as.character(db_transactions$campaign_id))
  db_transactions$date <- as.Date(db_transactions$transaction_date, format="%Y-%m-%d")
  db_transactions$day_of_week <- weekdays(as.Date(db_transactions$date,'%Y-%m-%d'))
  db_transactions$match_type <- as.match_type(db_transactions$match_type)
  db_transactions$keyword <- as.adwords.keyword(db_transactions$keyword)
  db_transactions <- db_transactions %>% date_filter(start_date, end_date)
  db_transactions <- db_transactions %>% select(-user_id)
  db_transactions <- db_transactions %>% mutate(user_id=as.integer(as.character(app_user_id)))

  # Add campaign names to db_transactions log
  db_transactions <- adwords_campaigns_data %>% 
                       group_by(campaign_id)%>% 
                       summarize(campaign_name = first(campaign_name)) %>%
                       right_join(db_transactions, by=c(campaign_id = "campaign_id"))


  ###################################### CREATE ELOGS ################################################
  # Create keywords elog
  keywords_elog <- rbind.fill(db_transactions, adwords_keywords_data)
  keywords_elog$week <- as.week(keywords_elog$date)
  keywords_elog <- keywords_elog %>% arrange(week)
}