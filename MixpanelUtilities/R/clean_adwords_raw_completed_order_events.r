#' Clean Adwords Raw Completed Order Eventss
#'
#' Cleans the data from Mixpanel related to AdWords completed order events.
#' @param adwords_events A data frame of events specifically relating to AdWords from \code{all_ppc_raw_completed_order_events} or \code{adwords_raw_completed_order_events}
#' @return A data frame with the events cleaned.
#' @export
#' @examples
#' clean_adwords_raw_completed_order_events(adwords_events)
clean_adwords_raw_completed_order_events  <- function(adwords_events){
	adwords_events <- data.frame(adwords_events)
	adwords_events  <- adwords_events %>% rename(app_user_id = id)
	adwords_events  <- adwords_events %>% mutate(app_user_id = as.numeric(as.character(app_user_id)))

	return(adwords_events)
}