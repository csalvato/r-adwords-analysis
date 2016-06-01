#' Keywords with Earnings
#'
#' Take in an adwords keyword event log and creates a data frame
#' containing only those keywords that have earnings.
#'
#' @param keywords_elog A data frame containing keywords and transaction event log data.
#' @param campaign_filter Optional. (e.g. "Paleo Performers") If provided, generates a data frame and plot for campaigns where the string is included in the campaign name. Default to NULL.
#' @param write_csv logical. If TRUE, creates a csv of the data frame in the working directory with the filename. If false, creates no file. Defaults to \code{TRUE}.
#' @param file String. The filename for the generated report if \code{write_csv} is set to \code{TRUE}. Defaults to adwords_keyword_campaign_device_matchtype_report.csv".
#' @return The data frame used to create the report.
#' @export
#' @examples
#' keywords_with_earnings(keywords_elog, keyword_filter="paleo meals", campaign_filter="Paleo Performers")

keywords_with_earnings <- function(keywords_elog,
                                   campaign_filter=NULL,
                                   write_csv = TRUE,
                                   file="adwords_keywords_report.csv"){
  require(plyr)
  require(dplyr)
  require(SalvatoUtilities)
  if( !is.null(campaign_filter) ) {
  	keywords_elog <-  keywords_elog %>% filter(grepl(campaign_filter,campaign_name))
  }

  data <- keywords_report(keywords_elog, 
                          campaign_filter=campaign_filter, 
                          write_csv=FALSE) %>% 
          filter(earnings > 0)

	if( write_csv ) {
		write.excel.csv(data, file)
	 }

	return(data)

}