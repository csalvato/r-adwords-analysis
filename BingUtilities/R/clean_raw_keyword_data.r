#' Clean Raw Bing Keyword Data
#'
#' Takes the raw data from an Bing Keyword Performance Report via an API pull and cleans/formats it
#' to be more easily readable, understandable and consistent with other data sets.
#' @param data_frame A data frame of raw data from an Bing Keyword Performance Report
#' @return A data frame with the Bing data in a format consistent with other related data sets.
#' @export
#' @examples
#' data  <- raw_keyword_performance_data(from=20151216, to=20151219)
#' clean_data <- clean_raw_keyword_data(data)


clean_raw_keyword_data  <- function(data_frame) {
  #TODO ADJUST THIS TO CLEAN BING DATA FROM CSV FILE

  # Reformat column names
  names(data_frame) <- gsub('\\(|\\)',"",tolower(names(data_frame)))
  
  # Rename columns, convert values where necessary, 
  # and filter everything outside of start_date and end_date
  require(plyr)
  require(dplyr)
  data_frame <- data_frame %>%
                rename( date=day,
                        day_of_week=dayofweek,
                        keyword_state=keywordstate,
                        campaign_id=campaignid,
                        campaign_name=campaign,
                        ad_group_id=adgroupid,
                        ad_group_name=adgroup,
                        network=networkwithsearchpartners,
                        est_search_impression_share=searchimpr.share,
                        est_search_impression_share_lost_rank=searchlostisrank,
                        average_position=position,
                        quality_score=qualityscore,
                        landing_page_experience=landingpageexperience,
                        match_type=matchtype) %>% 
                mutate(device = as.device(device),
                       keyword = as.adwords.keyword(keyword),
                       quality_score = as.numeric(quality_score)) %>% 
                date_filter(start_date, end_date)

  return(data_frame)
}