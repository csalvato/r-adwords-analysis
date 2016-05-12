#' Retrieve Mixpanel AdWords Completed Order Events
#'
#' Wrapper function to get completed order events originating from AdWords
#' @param account A mixpanel account object.
#' @param from Start date in either format <"yyyy-mm-dd"> or <yyyymmdd>. Inclusive.
#' @param to End date in either format <"yyyy-mm-dd"> or <yyyymmdd>. Inclusive.
#' @export
#' @examples
#' adwords_completed_order_events(account, from=start_date, to=end_date)
adwords_completed_order_events <- function(account, from=Sys.Date(), to=Sys.Date()) {
  return(
          mixpanelGetEvents(account, 
                            from = from,
                            to = to,
                            event = array("Completed Order"),
                            where = '(properties["latest_ad_search"]) and (properties["latest_ad_utm_source"] == "Google")')
        )
}