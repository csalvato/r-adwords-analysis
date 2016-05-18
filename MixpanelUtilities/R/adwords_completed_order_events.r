#' Retrieve Raw Mixpanel AdWords Completed Order Events
#'
#' Wrapper function to get completed order events originating from AdWords.  If an 
#' RMixpanel account object is not provided, the credentials are looked up using a
#' mixpanel_credentials.yml file in the working directory.  mixpanel_credentials.yml
#' must include four key value pairs: name, token, apiKey, apiSecret.
#' @param account A mixpanel account object.
#' @param from Start date in either format <"yyyy-mm-dd"> or <yyyymmdd>. Inclusive.
#' @param to End date in either format <"yyyy-mm-dd"> or <yyyymmdd>. Inclusive.
#' @export
#' @examples
#' adwords_raw_completed_order_events(account, from=start_date, to=end_date)
adwords_raw_completed_order_events <- function(account=NULL, from=Sys.Date(), to=Sys.Date()) {
  all_events <- all_ppc_completed_order_events(from=from, to=to)
  return(all_events[["adwords"]])
}