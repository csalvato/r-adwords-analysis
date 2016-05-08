#' String From File
#'
#' Reads a string literal from a file and performs string
#' interpolation on values from the string using `@{}` syntax. 
#' (e.g @{start_date})
#'
#' Should consider using Whisker... https://github.com/edwindj/whisker
#' @param file_path Path to the file containing the string.
#' @export
#' @examples
#' string_from_file('./transactions_query.sql')
string_from_file <- function(file_path){
  GetoptLong::qq(scan(file_path, character()))
}