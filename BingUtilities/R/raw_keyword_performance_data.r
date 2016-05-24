#' Get Raw Keyword Performance Data
#'
#' This is a utility function for that pulls a Keyword Performance Report
#' with the data needed for our analysis from a Bing CSV.
#' @param from Date object for start date in format <yyyymmdd>. Inclusive.  If left blank, defaults to Sys.Date()
#' @param to Date object for end date in format <yyyymmdd>. Inclusive. If left blank, defaults to Sys.Date()
#' @param keyword_performance_file_name Filename from which to pull the data.  Bing has a terrible API, so the file must be manually downloaded from https://reporting.bingads.microsoft.com until we figure out the API.
#' @return A data frame with the keyword performance data in the timeframe.
#' @export
#' @examples
#' raw_keyword_performance_data(from=20151216, to=20151219)
raw_keyword_performance_data <- function(from=Sys.Date(), 
																				 to=Sys.Date(),
																				 keyword_performance_file_name="bing_keyword_performance_report.csv") {  
  require(plyr)
  require(dplyr)

  data <- read.table( file=keyword_performance_file_name, 
  										header=TRUE,
  										sep=",", 
  										quote="\"", 
  										skip=9,
  										stringsAsFactors=FALSE) %>%
  					filter(Gregorian.date != "Ã‚Â©2016 Microsoft Corporation. All rights reserved. ") %>%
  				  mutate(Gregorian.date = as.Date(Gregorian.date, format="%m/%d/%Y")) %>%
  				  filter(Gregorian.date >= from & Gregorian.date <= to)

  return(data)

}