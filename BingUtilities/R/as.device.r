#' As Device
#'
#' This function converts the values in Bing vectors to dt for desktop or mb for mobile
#' @param device_vector A vector of device values from AdWords.
#' @export
#' @examples
#' as.device(device_vector)
#' as.device(google_adwords_report$device)

as.device <- function(device_vector) {
  device_vector <- gsub("Computer", "dt", device_vector)
  device_vector <- gsub("Tablet", "dt", device_vector)
  device_vector <- gsub("Smartphone", "mb", device_vector)
  return(device_vector)
}