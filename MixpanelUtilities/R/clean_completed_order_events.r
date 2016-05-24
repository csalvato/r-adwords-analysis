#' Clean Raw Completed Order Eventss
#'
#' Cleans the Completed Order events data from Mixpanel.
#' @param events_data_frame A matrix of events specifically relating to ppc events from Mixpanel generated by \code{all_ppc_raw_completed_order_events}, \code{adwords_raw_completed_order_events}, \code{bing_raw_completed_order_events}
#' @return A data frame with the events cleaned.
#' @export
#' @examples
#' clean_completed_order_events(adwords_events)
clean_completed_order_events  <- function(events_matrix){
	events_data_frame <- data.frame(events_matrix)
	events_data_frame  <- events_data_frame %>% rename(app_user_id = id)
	events_data_frame  <- events_data_frame %>% mutate(app_user_id = as.numeric(as.character(app_user_id)))

	#Clean up keywords info for future lookup of Keyword name with Keyword ID from Bing data.
	events_data_frame <- events_data_frame %>% mutate(keyword_id = as.numeric(gsub("kwd-","",latest_ad_awkeyword)))

	return(events_data_frame)
}