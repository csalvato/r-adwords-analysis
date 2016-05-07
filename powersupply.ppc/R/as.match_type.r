#' As Match Type
#'
#' This function converts the non-human-readable values in the matchtype
#' vector provided by Google AdWords valuetrak tags to a human-readable
#' version.
#' @param valuetrak_match_type_vector A vector of match type valuetrak values.
#' @export
#' @examples
#' as.match_type(valuetrak_match_type_vector)
#' as.match_type(google_adwords_report$match_type)
as.match_type <- function(valuetrak_match_type_vector) {
  valuetrak_match_type_vector <- gsub("b", "Broad", valuetrak_match_type_vector)
  valuetrak_match_type_vector <- gsub("e", "Exact", valuetrak_match_type_vector)
  valuetrak_match_type_vector <- gsub("p", "Phrase", valuetrak_match_type_vector)
  return(valuetrak_match_type_vector)
}