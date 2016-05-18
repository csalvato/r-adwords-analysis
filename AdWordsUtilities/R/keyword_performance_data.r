#' Get Keyword Performance Data
#'
#' This is a utility function for that pulls a Keyword Performance Report
#' with the data needed for our analysis. The data is returned cleaned so that it is
#' consistent with data formats from other source and easily human readable.
#' If a field is missing that is supported by the \code{keyword performance report}, 
#' it should be added to the function \code{raw_keyword_performance_data}
#' NOTE: Requires you to enter Google Authentication info or have
#' .google.auth.RData in your working directory.
#' @param from Date object for start date in format <yyyymmdd>. Inclusive.  If left blank, defaults to Sys.Date()
#' @param to Date object for end date in format <yyyymmdd>. Inclusive. If left blank, defaults to Sys.Date()
#' @return A data frame with the cleaned keyword performance data in the timeframe.
#' @export
#' @examples
#' keyword_performance_data(from=20151216, to=20151219)
keyword_performance_data <- function(from=Sys.Date(), to=Sys.Date()) {
  raw <- raw_keyword_performance_data(from, to)
  cleaned  <- clean_raw_adwords_keyword_data(raw)
  return(cleaned)
}