#' Create plot of Bing performance over time
#'
#' Takes in a Bing event log and processes all summary data
#'
#' @param keywords_elog A data frame containing keywords and transaction event log data.
#' @param plot logical. If TRUE, creates a plot of the data.  If false, creates no plot, Defaults to \code{TRUE}.
#' @param plot_png_file String with the path to the plot file that should be written. If NULL, no file is written. Defaults to \code{NULL}.
#' @return The data frame used to create a plot of performance over time.
#' @export
#' @examples
#' overall_performance_over_time(elog_data_frame)

overall_performance_over_time <- function(keywords_elog, plot = TRUE, plot_png_file=NULL){
  require(plyr)
  require(dplyr)
  all_keyword_ROAS_over_time <- keywords_elog %>%
															  group_by(week) %>%
															  BingUtilities::summarize_elog() %>%
															  mutate(cum_contribution = cumsum(contribution),
															         cum_cost = cumsum(cost),
															         cum_ROAS = cum_contribution - cum_cost) %>%
															  gather(type,value,cum_cost,cum_contribution, cum_ROAS) %>%
															  select(week, type, value)

	if( plot ) {
		require(ggplot2)
		the_plot <- ggplot(all_keyword_ROAS_over_time, 
		            aes(week,value,group=type,col=type,fill=type)) + 
		     geom_line()
		report.plot( the_plot, plot_png_file)
	 }

	return(all_keyword_ROAS_over_time)

}