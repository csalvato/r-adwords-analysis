#' Keywords Report
#'
#' Take in an Bing keyword event log and creates a report
#' calculating metrics and grouping by Keyword.
#'
#' @param keywords_elog A data frame containing keywords and transaction event log data.
#' @param keyword_filter Optional. (e.g. "paleo meals") If provided, generates a data frame and plot only for the *exact* keyword provided. Default to NULL.
#' @param campaign_filter Optional. (e.g. "Paleo Performers") If provided, generates a data frame and plot for campaigns where the string is included in the campaign name. Default to NULL.
#' @param write_csv logical. If TRUE, creates a csv of the data frame in the working directory with the filename. If false, creates no file. Defaults to \code{TRUE}.
#' @param file String. The filename for the generated report if \code{write_csv} is set to \code{TRUE}. Defaults to adwords_keyword_campaign_device_matchtype_report.csv".
#' @return The data frame used to create the report.
#' @export
#' @examples
#' keywords_overview(keywords_elog, keyword_filter="paleo meals", campaign_filter="Paleo Performers")

keywords_report <- function(keywords_elog,
                              keyword_filter=NULL,
                              campaign_filter=NULL,
                              write_csv = TRUE,
                              file="bing_keywords_report.csv"){
  require(plyr)
  require(dplyr)
  require(SalvatoUtilities)
  if( !is.null(keyword_filter) ) {
  	keywords_elog <- keywords_elog %>% filter(keyword==keyword_filter)
  } 
  if( !is.null(campaign_filter) ) {
  	keywords_elog <-  keywords_elog %>% filter(grepl(campaign_filter,campaign_name))
  }

  data <- bing_keywords_elog %>%
          group_by(keyword) %>%
          BingUtilities::summarize_elog()

	if( write_csv ) {
		write.excel.csv(data, file)
	 }

	return(data)

}