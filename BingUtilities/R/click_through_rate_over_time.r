#' Create plot of Bing click through rate over time
#'
#' Takes in a Bing event log and processes click through rate over time.
#'
#' @param keywords_elog A data frame containing keywords and transaction event log data.
#' @param plot logical. If TRUE, creates a plot of the data.  If false, creates no plot, Defaults to \code{TRUE}.
#' @return The data frame used to create a plot of clickthrough rate over time.
#' @export
#' @examples
#' overall_performance_over_time(elog_data_frame)

click_through_rate_over_time <- function(keywords_elog, plot = TRUE){
  require(plyr)
  require(dplyr)
  keywords_weekly_conversion_metrics <- keywords_elog %>%
																			  group_by(keyword, campaign_name, week) %>%
																			  BingUtilities::summarize_elog() %>%
																			  #filter(grepl("Paleo Performers",campaign_name)) %>%
																			  ungroup %>%
																			  arrange(keyword, campaign_name, week) %>%
																			  select(keyword, campaign_name, week, impressions, clicks, num_acquisitions, click_through_rate, conversion_rate, cost_per_click, contribution_per_click)

	if( plot ) {
		require(ggplot2)
		plot( 
		  ggplot(
		    keywords_weekly_conversion_metrics %>% 
		      # Filter by a single keyword, and only include the previous 4 weeks of data.
		      filter(week >= Sys.Date() - weeks(4), week <= Sys.Date(), keyword == "paleo meals"), 
		    aes(x=week, y=click_through_rate, fill=campaign_name)) +
		    geom_bar(stat="identity", position="dodge") +
		    ggtitle("Bing - Weekly CTR by Keyword") +
		    # ylim(0, 0.30) +
		    facet_wrap(~keyword, ncol=2)
		)
	}

	return(keywords_weekly_conversion_metrics)

}