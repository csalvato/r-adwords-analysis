#' As Device
#'
#' This function converts the values in AdWords vectors to dt for desktop or mb for mobile
#' @param device_vectory A vector of device values from AdWords.
#' @export
#' @examples
#' as.device(device_vector)
#' as.device(google_adwords_report$device)

as.device <- function(device_vector) {
  device_vector <- gsub("Computers", "dt", device_vector)
  device_vector <- gsub("Tablets with full browsers", "dt", device_vector)
  device_vector <- gsub("Mobile devices with full browsers", "mb", device_vector)
  return(device_vector)
}