#' Create Campaign Lookup Table
#'
#' Creates a lookup table for all campaign names that exist within a date range.  
#' That is, creates one column for campaign name, and another column with campaign ID. 
#' @param from Start date in either format <"yyyy-mm-dd"> or <yyyymmdd>. Inclusive.
#' @param to End date in either format <"yyyy-mm-dd"> or <yyyymmdd>. Inclusive.
#' @return A data frame with a lookup table of campaign IDs and campaign names.
#' @export
#' @examples
#' campaign_lookup_table(from=20150101, to=20151231)
campaign_lookup_table  <- function(from=Sys.Date(), to=Sys.Date()){
  adwords_campaigns_data <- campaign_performance_data(from=as.Date(from), to=as.Date(to))
  lookup_table  <- adwords_campaigns_data %>% 
                       group_by(campaign_id)%>% 
                       summarize(campaign_name = first(campaign_name))

	return(lookup_table)
}