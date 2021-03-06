#' Create plot of Bing keyword performance over time
#'
#' Takes in a Bing event log and processes keyword summary data into a plot
#'
#' @param keywords_elog A data frame containing keywords and transaction event log data.
#' @param plot logical. If TRUE, creates a plot of the data.  If false, creates no plot, Defaults to \code{TRUE}.
#' @param plot_png_file String with the path to the plot file that should be written. If NULL, no file is written. Defaults to \code{NULL}.
#' @return The data frame used to create a plot of performance over time.
#' @export
#' @examples
#' keywords_performance_over_time(elog_data_frame)

keywords_performance_over_time <- function(keywords_elog, plot = TRUE, plot_png_file=NULL){
  require(plyr)
  require(dplyr)
  
  earnings_keywords <- keywords_with_earnings(keywords_elog, write_csv = FALSE)
  
  keywords_over_time <- keywords_elog %>%
											  filter(keyword %in% earnings_keywords$keyword) %>%
											  group_by(keyword,week) %>%
											  summarize(cost = sum(cost, na.rm = TRUE),
											            contribution = sum(money_in_the_bank_paid_to_us,na.rm=TRUE) *.25) %>%
											  mutate(cum_contribution = cumsum(contribution), 
											         cum_cost = cumsum(cost),
											         cum_ROI = cum_contribution - cum_cost) %>%
											  gather(type,value,cum_cost,cum_contribution,cum_ROI)

	if( plot ) {
		require(ggplot2)
		the_plot <- ggplot(keywords_over_time, aes(week,value,group=type,col=type,fill=type)) + 
								       geom_line() + 
								       ggtitle("Bing - Keyword Trends") + 
								       facet_wrap(~keyword, ncol=2)
		report.plot(the_plot, plot_png_file)
	 }

	return(keywords_over_time)

}