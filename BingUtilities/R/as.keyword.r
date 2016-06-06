#' As Keyword
#'
#' This function formats Bing keywords data by removing special characters and ensuring proper typing.
#' @param keyword_vectoro A vector of keywords from Bing
#' @export
#' @examples
#' as.keyword(keyword_vector)
#' as.keyword(bing_report$keyword)
as.keyword <- function(keyword_vector) {
  keyword_vector <- as.character(keyword_vector)
  keyword_vector <- tolower(keyword_vector)
  keyword_vector <- gsub("\\+","",keyword_vector)
  return(keyword_vector)
}