#' Date Filter
#'
#' This function takes a data_frame with a date field (as strings) and
#' filters the data_frame to just show dates within the start_date
#' and end_date, inclusively.
#'
#' @param data_frame A data frame containing a `date` column
#' @param start_date A date object representing the start date. Defaults to today.
#' @param end_date A date object representing the end date. Defaults to today.
#' @export
#' @examples
#' data <- data.frame(date = as.Date(c("2015-12-16", "2016-02-01", "2017-03-01")), 
#'									a = c("1","2","3"), 
#'									b = c("3","2","1"))
#'         date a b
#' 1 2015-12-16 1 3
#' 2 2016-02-01 2 2
#' 3 2017-03-01 3 1
#'
#' date_filter(data, as.Date("2016-01-01"), as.Date("2016-02-02"))
#'         date a b
#' 1 2016-02-01 2 2
library(dplyr)

date_filter <- function(data_frame, start_date=Sys.Date(), end_date=Sys.Date()) {
  return(data_frame %>% filter(date >= start_date, date <= end_date))
}