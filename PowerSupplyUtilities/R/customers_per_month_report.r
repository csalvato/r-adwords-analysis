#' Customers per month report
#'
#' Take in an adwords keyword event log and creates a report
#' showing the total number of customers per month.
#'
#' @param keywords_elog A data frame containing keywords and transaction event log data.
#' @param keyword_filter Optional. (e.g. "paleo meals") If provided, generates a data frame and plot only for the *exact* keyword provided. Default to NULL.
#' @param campaign_filter Optional. (e.g. "Paleo Performers") If provided, generates a data frame and plot for campaigns where the string is included in the campaign name. Default to NULL.
#' @param write_csv logical. If TRUE, creates a csv of the data frame in the working directory with the filename. If false, creates no file. Defaults to \code{TRUE}.
#' @param file String. The filename for the generated report if \code{write_csv} is set to \code{TRUE}. Defaults to "customers_per_month_report.csv".
#' @return The data frame used to create the report.
#' @export
#' @examples
#' contribution_per_click_report(keywords_elog, keyword_filter="paleo meals", campaign_filter="Paleo Performers")

customers_per_month_report <- function(keywords_elog,
                                        keyword_filter=NULL,
                                        campaign_filter=NULL,
                                        write_csv = TRUE,
                                        file="customers_per_month_report.csv"){
  require(plyr)
  require(dplyr)
  require(SalvatoUtilities)
  if( !is.null(keyword_filter) ) {
  	keywords_elog <- keywords_elog %>% filter(keyword==keyword_filter)
  } 
  if( !is.null(campaign_filter) ) {
  	keywords_elog <-  keywords_elog %>% filter(grepl(campaign_filter,campaign_name))
  }

  data <- keywords_elog %>% 
            mutate(sortable_date=format(week, format="%Y-%m-01")) %>% 
            filter(!is.na(user_id)) %>% 
            group_by(user_id) %>% 
            summarize(first_transaction_month=as.Date(format(min(sortable_date, na.rm=TRUE)))) %>%
            group_by(first_transaction_month) %>% 
            summarize(num_acquisitions = n_distinct(user_id, na.rm = TRUE)) %>% 
            mutate(first_transaction_month=format(first_transaction_month,format="%Y-%m"))

	if( write_csv ) {
		write.excel.csv(data, file)
	 }

	return(data)

}

