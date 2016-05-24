#' Clean Raw Bing Keyword Data
#'
#' Takes the raw data from an Bing Keyword Performance Report via an API pull and cleans/formats it
#' to be more easily readable, understandable and consistent with other data sets.
#' @param data_frame A data frame of raw data from an Bing Keyword Performance Report
#' @return A data frame with the Bing data in a format consistent with other related data sets.
#' @export
#' @examples
#' data  <- raw_keyword_performance_data(from=20151216, to=20151219)
#' clean_raw_keyword_data <- clean_raw_keyword_data(data)


clean_raw_keyword_data  <- function(data_frame) {
  # Reformat column names
  names(data_frame) <- gsub('\\(|\\)',"",tolower(names(data_frame)))
  
  # Rename columns, convert values where necessary, 
  # and filter everything outside of start_date and end_date
  require(plyr)
  require(dplyr)
  data_frame <- data_frame %>%
                rename( date=gregorian.date,
                        est_search_impression_share=impression.share....,
                        est_search_impression_share_lost_rank=impression.share.lost.to.rank....,
                        keyword_id = keyword.id,
                        keyword_state=keyword.status,
                        campaign_id=campaign.id,
                        campaign_name=campaign.name,
                        ad_group_id=ad.group.id,
                        ad_group_name=ad.group,
                        cost=spend,
                        device=device.type,
                        average_position=avg..position,
                        quality_score=quality.score,
                        match_type=delivered.match.type,
                        landing_page_experience=landing.page.user.experience) %>% 
                mutate(day_of_week=weekdays(date),
                       keyword=BingUtilities::as.keyword(keyword)) %>% 
                # mutate(device = as.device(device),
                #        quality_score = as.numeric(quality_score)) %>% 
                date_filter(start_date, end_date)

  return(data_frame)
}