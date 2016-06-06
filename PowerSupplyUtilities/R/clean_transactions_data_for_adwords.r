#' Clean Raw Transaction Data from Application Database
#'
#' Cleans the transactions data from the App database for AdWords
#' @param data_frame A data frame of transaction events from \code{get_transactions_data}
#' @return A data frame with the transaction events cleaned and consistent with other data frames.
#' @export
#' @examples
#' clean_transactions_data(data_frame)
clean_transactions_data_for_adwords  <- function(data_frame){
  require(plyr)
  require(dplyr)
  require(AdWordsUtilities)
  data_frame <- data_frame %>% 
                  rename( device=latest_ad_device, 
                          campaign_id=latest_ad_utm_campaign,
                          keyword=latest_ad_awkeyword,
                          ad_group_id=latest_ad_awadgroupid,
                          match_type=latest_ad_awmatchtype) %>%
                  mutate( campaign_id = as.integer(as.character(campaign_id)),
                          date = as.Date(transaction_date, format="%Y-%m-%d"),
                          day_of_week = weekdays(as.Date(date,'%Y-%m-%d')),
                          match_type = as.match_type(match_type),
                          keyword = as.adwords.keyword(keyword),
                          user_id=as.integer(as.character(app_user_id))) %>%
                  date_filter(start_date, end_date) %>%
                  select(-app_user_id) %>%
                  mutate()

	return(data_frame)
}