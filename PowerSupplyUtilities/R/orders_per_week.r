#' Create plot of number of orders per week by geo
#'
#' Takes in a AdWords event log and processes all summary data
#'
#' @param keywords_elog A data frame containing keywords and transaction event log data.
#' @param keyword_filter Optional. (e.g. "paleo meals") If provided, generates a data frame and plot only for the *exact* keyword provided. Default to NULL.
#' @param campaign_filter Optional. (e.g. "Paleo Performers") If provided, generates a data frame and plot for campaigns where the string is included in the campaign name. Default to NULL.
#' @param plot logical. If TRUE, creates a plot of the data.  If false, creates no plot, Defaults to \code{TRUE}.
#' @param plot_png_file String with the path to the plot file that should be written. If NULL, no file is written. Defaults to \code{NULL}.
#' @return The data frame used to create a plot of performance over time.
#' @export
#' @examples
#' orders_per_week(keywords_elog, keyword_filter="paleo meals", campaign_filter="Paleo Performers")

orders_per_week <- function(keywords_elog, 
														keyword_filter=NULL, 
														campaign_filter=NULL,
														plot = TRUE,
                            plot_png_file=NULL){
  require(plyr)
  require(dplyr)
  if( !is.null(keyword_filter) ) {
  	keywords_elog <- keywords_elog %>% filter(keyword==keyword_filter)
  } 
  if( !is.null(campaign_filter) ) {
  	keywords_elog <-  keywords_elog %>% filter(grepl(campaign_filter,campaign_name))
  }

  num_orders_per_week <- keywords_elog %>% 
                        group_by(week,campaign_name) %>% 
                        summarize(cost = sum(cost, na.rm=TRUE), 
                                  contribution = sum(money_in_the_bank_paid_to_us, 
                                                     na.rm=TRUE)*0.25, 
                                  num_orders=n_distinct(user_name, na.rm=TRUE))

	if( plot ) {
    require(ggplot2)
		the_plot <- ggplot( num_orders_per_week ) + 
      aes(week,num_orders,group=campaign_name,col=campaign_name,fill=campaign_name) + 
      geom_line() +
      ylim(0,max(num_orders_per_week$num_orders))+
      facet_wrap(~campaign_name, ncol=2)
    
    report.plot( the_plot, plot_png_file)
	 }

	return(num_orders_per_week)

}