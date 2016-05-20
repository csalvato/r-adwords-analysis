#' Get Raw Keyword Performance Data
#'
#' This is a utility function for that pulls a Keyword Performance Report
#' with the data needed for our analysis from a Bing CSV.
#' @param from Date object for start date in format <yyyymmdd>. Inclusive.  If left blank, defaults to Sys.Date()
#' @param to Date object for end date in format <yyyymmdd>. Inclusive. If left blank, defaults to Sys.Date()
#' @return A data frame with the keyword performance data in the timeframe.
#' @export
#' @examples
#' raw_keyword_performance_data(from=20151216, to=20151219)
raw_keyword_performance_data <- function(from=Sys.Date(), to=Sys.Date()) {  
  
  #TODO pull data from downloaded CSV file (The API is too much of a headache right now.)
  #data <- 

  return(data)

}