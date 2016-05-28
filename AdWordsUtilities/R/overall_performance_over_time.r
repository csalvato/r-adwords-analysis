#' Create plot of AdWords performance over time
#'
#' Takes in a AdWords event log and processes all summary data
#'
#' @param keywords_elog A data frame containing keywords and transaction event log data.
#' @return plot logical. If TRUE, creates a plot of the data.  If false, creates no plot, Defaults to \code{TRUE}.
#' @return The data frame used to create a plot of performance over time.
#' @export
#' @examples
#' overall_performance_over_time(elog_data_frame)

overall_performance_over_time <- function(keywords_elog, plot = TRUE){
  require(plyr)
  require(dplyr)
  all_keyword_ROAS_over_time <- keywords_elog %>%
															  group_by(week) %>%
															  AdWordsUtilities::summarize_elog() %>%
															  mutate(cum_contribution = cumsum(contribution),
															         cum_cost = cumsum(cost),
															         cum_ROAS = cum_contribution - cum_cost) %>%
															  gather(type,value,cum_cost,cum_contribution, cum_ROAS) %>%
															  select(week, type, value)

	if( plot ) {
		plot(ggplot(all_keyword_ROAS_over_time, 
		            aes(week,value,group=type,col=type,fill=type)) + 
		     geom_line()
		    )
	 }

	return(all_keyword_ROAS_over_time)

}