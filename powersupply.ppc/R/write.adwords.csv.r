#' Write Excel CSV
#'
#' Wrapper function to write data frame to an excel-readable CSV file.
#'
#' @param data_frame Data frame to be written to CSV file.
#' @param file The path to the file to be written
#' @export
#' @examples
#' write.excel.csv(data_frame, "some_csv_file.csv")

write.excel.csv <- function(data_frame, file){
  write.csv( data_frame,
             file=file,
             eol = "\r\n", 
             row.names=FALSE)
}