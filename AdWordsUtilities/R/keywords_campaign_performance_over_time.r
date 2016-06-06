#' Create plot of AdWords keyword performance over time, grouped by campaign
#'
#' Takes in a AdWords event log and processes keyword and campaign summary data into a plot
#'
#' @param keywords_elog A data frame containing keywords and transaction event log data.
#' @param plot logical. If TRUE, creates a plot of the data.  If false, creates no plot, Defaults to \code{TRUE}.
#' @return The data frame used to create a plot of performance over time.
#' @export
#' @examples
#' keywords_campaign_performance_over_time(elog_data_frame)

keywords_campaign_performance_over_time <- function(keywords_elog, plot = TRUE){
  require(plyr)
  require(dplyr)
  
  earnings_keywords <- keywords_with_earnings(keywords_elog, write_csv = FALSE)
  
  keywords_campaigns_over_time <- keywords_elog %>%
																  filter(keyword %in% earnings_keywords$keyword) %>%
																  #filter(grepl("Paleo Performers",campaign_name)) %>%
																  group_by(keyword, campaign_name, week) %>%
																  summarize(cost = sum(cost, na.rm = TRUE),
																            contribution = sum(money_in_the_bank_paid_to_us,na.rm=TRUE) *.25) %>%
																  mutate(cum_contribution = cumsum(contribution), 
																         cum_cost = cumsum(cost),
																         cum_ROI = cum_contribution - cum_cost) %>%
																  gather(type,value,cum_cost,cum_contribution,cum_ROI)

	if( plot ) {
		require(ggplot2)
		plot(ggplot(keywords_campaigns_over_time %>% filter(keyword == "paleo meals" & grepl("Paleo Performers",campaign_name)), aes(week,value,group=type,col=type,fill=type)) + 
       geom_line() + 
       ggtitle("AdWords - Keyword Trends by Campaign") + 
       facet_wrap(~keyword + campaign_name, ncol=2))
	 }

	return(keywords_campaigns_over_time)

}