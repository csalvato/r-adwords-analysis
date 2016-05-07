write.adwords.csv <- function(data_frame, file){
  write.csv( data_frame,
             file=file,
             eol = "\r\n", 
             row.names=FALSE)
}