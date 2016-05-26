#' Contribution per click report
#'
#' Take in a bing keyword event log and creates a contribution per click report
#' by keyword and campaign_name
#'
#' @param keywords_elog A data frame containing keywords and transaction event log data.
#' @param keyword_filter Optional. (e.g. "paleo meals") If provided, generates a data frame and plot only for the *exact* keyword provided. Default to NULL.
#' @param campaign_filter Optional. (e.g. "Paleo Performers") If provided, generates a data frame and plot for campaigns where the string is included in the campaign name. Default to NULL.
#' @param write_csv logical. If TRUE, creates a csv of the data frame in the working directory with the filename. If false, creates no file. Defaults to \code{TRUE}.
#' @param file String. The filename for the generated report if \code{write_csv} is set to \code{TRUE}. Defaults to "bing_contribution_per_click_report.csv".
#' @return The data frame used to create the report.
#' @export
#' @examples
#' contribution_per_click_report(keywords_elog, keyword_filter="paleo meals", campaign_filter="Paleo Performers")

contribution_per_click_report <- function(keywords_elog,
                                          keyword_filter=NULL,
                                          campaign_filter=NULL,
                                          write_csv = TRUE,
                                          file="adwords_contribution_per_click_report.csv"){
  require(plyr)
  require(dplyr)
  if( !is.null(keyword_filter) ) {
  	keywords_elog <- keywords_elog %>% filter(keyword==keyword_filter)
  } 
  if( !is.null(campaign_filter) ) {
  	keywords_elog <-  keywords_elog %>% filter(grepl(campaign_filter,campaign_name))
  }

  data <- keywords_elog %>% 
            group_by(keyword,campaign_name) %>% 
            AdWordsUtilities::summarize_elog() %>% 
            filter(cost > 0 & earnings > 0) %>% 
            group_by(keyword,campaign_name) %>% 
            summarize(total_cost = sum(cost),
                      total_contribution = sum(contribution),
                      total_clicks = sum(clicks), 
                      contribution_per_click = total_contribution/total_clicks,
                      cpc_bid_for_2x_ROAS = contribution_per_click/2)

	if( write_csv ) {
		write.excel.csv(data, file)
	 }

	return(data)

}