#' Retrieve Mixpanel Completed Order Events From All PPC Channels
#'
#' Retrieves all ppc events (those with latest_ad_utm_source of "cpc") from Mixpanel.
#' If data with the start and end date specified has already been retrieved, it restores
#' the data from a local RData file, mixpanel_ppc_events.RData to save time.  
#' If data must be pulled, and an RMixpanel account object is not provided, the credentials
#' are looked up using a mixpanel_credentials.yml file in the working directory.  
#' mixpanel_credentials.yml must include four key value pairs: name, token, apiKey, apiSecret.
#' @param account A mixpanel account object.
#' @param from Start date in either format <"yyyy-mm-dd"> or <yyyymmdd>. Inclusive.
#' @param to End date in either format <"yyyy-mm-dd"> or <yyyymmdd>. Inclusive.
#' @export
#' @examples
#' bing_completed_order_events(account, from=start_date, to=end_date)
all_ppc_completed_order_events <- function(account=NULL, from=Sys.Date(), to=Sys.Date()) {
  mixpanel_events_file_name <- "mixpanel_ppc_events.RData"

  require(RMixpanel)
  require(plyr)
  require(dplyr)

  if(file.exists(mixpanel_events_file_name)){
    load("mixpanel_ppc_events.RData") # Creates/Overwrites ppc_events in environment
  }

  # If the start date or end date are different from the saved data, retrieve again.
  write(!file.exists(mixpanel_events_file_name) || 
       ppc_events[["start_date"]] != from  ||  
       ppc_events[["end_date"]] != to , stderr())
  write(!file.exists(mixpanel_events_file_name), stderr())
  write(ppc_events[["start_date"]] != from, stderr())
  write(ppc_events[["end_date"]] != to, stderr())

  write(paste("file exists? ", file.exists(mixpanel_events_file_name)), stderr())
  write(paste("start_date: ", ppc_events[["start_date"]]), stderr())
  write(paste("end_date: ", ppc_events[["end_date"]]), stderr())

  if( !file.exists(mixpanel_events_file_name) || 
       ppc_events[["start_date"]] != from  ||  
       ppc_events[["end_date"]] != to ) {
    
    if( is.null(account) ) {
      require(yaml)
      require(readr)
      account <- yaml.load(read_file("mixpanel_credentials.yml"))
    }

    ppc_events <- mixpanelGetEvents(account, 
                                from = from,
                                to = to,
                                event = array("Completed Order"),
                                where = '(properties["latest_ad_utm_medium"] == "cpc")')

    ppc_events <- data.frame(ppc_events)

    ppc_events <- list(start_date=from, end_date=to, events=ppc_events)
    save(ppc_events, file=mixpanel_events_file_name)
  }

  adwords_events <- ppc_events[['events']] %>% 
                          filter( latest_ad_utm_source == "Google")

  bing_events <- ppc_events[['events']] %>% 
                      filter( latest_ad_utm_source == "Bing")

  return( list(adwords=adwords_events, bing=bing_events) )
}