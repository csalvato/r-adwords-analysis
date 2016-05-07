
as.adwords.keyword <- function(keyword_vector) {
  keyword_vector <- as.character(keyword_vector)
  keyword_vector <- tolower(keyword_vector)
  keyword_vector <- gsub("\\+","",keyword_vector)
  # Handles Tag Manager not properly parsing the + in the keyword (by manually inserting it to all entries)
  # This is NOT a sustainable solution.
  # If the keyword field contains a plus, assume the first keyword is also broad match, and prepend a +
  keyword_vector <- ifelse(grepl("\\+", keyword_vector), paste('+',keyword_vector, sep=""), keyword_vector) 
  # Replace double spaces that started appearing in data.  
  #Not sure where they come from, so this monkey patches/hardcodes a fix.
  keyword_vector <- gsub("  ", " ", keyword_vector)
  return(keyword_vector)
}