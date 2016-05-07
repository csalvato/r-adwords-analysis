date_filter <- function(data_frame, start_date, end_date) {
  return(data_frame %>% filter(date >= start_date, date <= end_date))
}