#' Write Excel CSV
#'
#' Wrapper function to write data frame to an excel-readable CSV file.
#' Given just the data_frame, a csv file will be written to the working
#' directory.
#'
#' @param data_frame Data frame to be written to CSV file.
#' @param file (optional) File name for the file to be saved.  If not supplied
#' then the data frame's name is used.  Note, ".csv" is appended automatically.
#' @export
#' @examples
#' write.excel.csv(data_frame, "some_csv_file.csv")

write.excel.csv <- function(data_frame, file=""){
  # If the file name is not specified, use the data frame variable name.
  if( file == "" ) {
  	file <- deparse(substitute(data_frame)) #gets variable name as string	
  }

  # If doesn't have .csv extension in the name, add it.
  if ( !grepl("\\.csv$", file) ) {
  	 file <- paste0(file,".csv")
 	}

  write.csv( data_frame,
             file=file,
             eol = "\r\n", 
             row.names=FALSE)
}