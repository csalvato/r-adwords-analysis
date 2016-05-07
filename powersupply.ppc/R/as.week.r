as.week <- function(date_vector){
  floor_date(date_vector, "week") + days(1)
}