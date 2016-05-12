#' Summarize AdWords Event Log
#'
#' Takes in an AdWords event log and processes all summary data
#'
#' @param elog_data_frame A data frame containing all of the AdWords Event Log data.
#' @param margin Estimated percentage of contribution margin (e.g. 0.25 for 25%). Used in contribution and estimated Return on Ad Spend (ROAS) calculations
#' @param average_num_orders_in_lifetime The number of orders a new customer makes, on average. Used in estimated Return on Ad Spend (ROAS) calculations
#' @param average_value_per_order The average value per order.  Used in estimated Return on Ad Spend (ROAS) calculations
#' @export
#' @examples
#' summarize_adwords_elog(elog_data_frame)

summarize_adwords_elog <- function(elog_data_frame, 
                                   margin=0.25, 
                                   average_num_orders_in_lifetime=10,
                                   average_value_per_order=70
                                   ){
  return (summarize(elog_data_frame, cost = sum(cost, na.rm = TRUE),
                    average_position = weighted.mean(average_position,impressions, na.rm=TRUE),
                    average_quality_score=mean(quality_score, na.rm=TRUE),
                    estimated_available_impressions = sum(impressions/est_search_impression_share, na.rm=TRUE),
                    impressions = sum(impressions, na.rm = TRUE),
                    # imp share is not wholly accurate because of the way the numbers are reported, but close enough. 
                    # May result in 100%+ when impressions are very low 
                    est_search_impression_share = impressions/estimated_available_impressions,
                    clicks = sum(clicks, na.rm = TRUE),
                    click_through_rate = clicks/impressions,
                    num_acquisitions = n_distinct(user_id, na.rm = TRUE),
                    conversion_rate = num_acquisitions/clicks,
                    cost_per_click = cost/clicks,
                    earnings = sum(money_in_the_bank_paid_to_us, na.rm = TRUE),
                    contribution = earnings * margin,
                    earnings_per_click = earnings/clicks,
                    contribution_per_click= contribution/clicks,
                    cpa = ifelse(num_acquisitions==0, cost, cost/num_acquisitions),
                    referred_users=sum(new_referred_users, na.rm=TRUE),
                    referred_earnings=sum(referred_users_transaction_amount,na.rm=TRUE),
                    estimated_ltv = num_acquisitions*average_num_orders_in_lifetime*average_value_per_order*margin,
                    estimated_lifetime_ROAS=(estimated_ltv-cost)/cost,
                    total_earnings = earnings + referred_earnings,
                    total_contribution = total_earnings * margin,
                    actual_ROAS = (total_contribution-cost)/cost)
          )
}