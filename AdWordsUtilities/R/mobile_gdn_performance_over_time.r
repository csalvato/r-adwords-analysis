#' Create plot of Google Display Network mobile performance over time
#'
#' Takes in a AdWords event log and processes performance on mobile devices over time.
#'
#' @param gdn_elog A data frame containing keywords and transaction event log data.
#' @param campaign_filter Optional. (e.g. "Paleo Performers") If provided, generates a data frame and plot for campaigns where the string is included in the campaign name. Default to NULL.
#' @param plot_png_file String with the path to the plot file that should be written. If NULL, no file is written. Defaults to \code{NULL}.
#' @param plot logical. If TRUE, creates a plot of the data.  If false, creates no plot, Defaults to \code{TRUE}.
#' @return The data frame used to create a plot of mobile performance over time.
#' @export
#' @examples
#' overall_performance_over_time(elog_data_frame)

mobile_gdn_performance_over_time <- function( gdn_elog, 
                                              campaign_filter=NULL,
                                              plot = TRUE,
                                              plot_png_file=NULL){
  require(plyr)
  require(dplyr)
  require(SalvatoUtilities)
  if( !is.null(campaign_filter) ) {
    gdn_elog <-  gdn_elog %>% filter(grepl(campaign_filter,campaign_name))
  }
  devices_over_time <- gdn_elog %>%
                        group_by(device,week) %>%
                        summarize(cost = sum(cost, na.rm = TRUE),
                                  contribution = sum(money_in_the_bank_paid_to_us,na.rm=TRUE) *.25) %>%
                        mutate(cum_contribution = cumsum(contribution), 
                               cum_cost = cumsum(cost),
                               cum_ROI = cum_contribution - cum_cost) %>%
                        gather(type,value,cum_cost,cum_contribution,cum_ROI)

	if( plot ) {
		require(ggplot2)
    the_plot  <- ggplot(devices_over_time %>% 
                filter(device == "mb"), 
            aes(week,value,group=type,col=type,fill=type)) + 
             geom_line() + 
             ggtitle("GDN Trends on Mobile")
		report.plot( the_plot, plot_png_file )
	}

	return(devices_over_time)

}