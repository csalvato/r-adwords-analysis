#' As Week
#'
#' This function converts a vector of date objects 
#' (whose day may be any time within Sunday to Monday)
#' of the week, into the date for the Monday of that week.
#' @param date_vector A vector of dates.
#' @export
#' @examples
#' date_vector <- (c(Sys.Date()))
#' date_vector
#' [1] "2016-05-07" #Thursday
#' as.week(date_vector)
#' [1] "2016-05-02" # Preceeding Monday of the same week
#'
#' date_vector <- (c(Sys.Date()))
#' date_vector
#' [1] "2016-05-01" # Sunday
#' as.week(date_vector)
#' [1] "2016-05-02" # Following monday of the same week

as.week <- function(date_vector){
  floor_date(date_vector, "week") + days(1)
}