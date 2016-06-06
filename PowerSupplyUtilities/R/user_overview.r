#' Generate User Performance Overview
#'
#' Takes in a AdWords or Bing event log and creates a revenue breakdown by users
#'
#' @param keywords_elog A data frame containing keywords and transaction event log data.
#' @return The data frame used to create a plot of performance over time.
#' @export

user_overview <- function(keywords_elog){
  require(plyr)
  require(dplyr)
  user_overview <- keywords_elog %>% 
                  filter(!is.na(user_id)) %>% #Remove NA user_ids (which means they are not monetary transactions)
                  group_by(user_id) %>%
                  summarize(name = first(user_name), 
                            num_transactions=length(date), 
                            earnings = sum(money_in_the_bank_paid_to_us, na.rm=TRUE),
                            contribution = earnings*.25,
                            campaign_name = first(campaign_name),
                            keyword=first(keyword),
                            referred_users=sum(new_referred_users, na.rm=TRUE),
                            referred_earnings=sum(referred_users_transaction_amount,na.rm=TRUE),
                            referred_contribution = referred_earnings * .25,
                            total_earnings = earnings + referred_earnings,
                            total_contribution = contribution + referred_contribution)

  return(user_overview)

}