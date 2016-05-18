#' Clean Raw AdWords Campaign Data
#'
#' Takes the raw data from an AdWords campaign Performance Report via an API pull and cleans/formats it
#' to be more easily readable, understandable and consistent with other data sets.
#' @param data_frame A data frame of raw data from an AdWords Campaign Performance Report via an API pull
#' @return A data frame with the AdWords data in a format consistent with other related data sets.
#' @export
#' @examples
#' data  <- campaign_performance_data(from=20151216, to=20151219)
#' clean_data <- clean_raw_adwords_campaign_data(data)


clean_raw_adwords_campaign_data  <- function(data_frame) {
  # Format AdWords campaign data for future use
  names(data_frame) <- gsub('\\(|\\)',"",tolower(names(data_frame)))

  data_frame <- data_frame %>%
                rename( date=day,
                        day_of_week=dayofweek,
                        campaign_state=campaignstate,
                        campaign_id=campaignid,
                        campaign_name=campaign,
                        network=networkwithsearchpartners,
                        est_search_impression_share=searchimpr.share,
                        est_search_impression_share_lost_rank=searchlostisrank,
                        est_search_impression_share_lost_budget=searchlostisbudget,
                        average_position=position) %>%
                  mutate(campaign_id = as.integer(campaign_id),
                         device = as.device(device)) %>% 
                  date_filter(start_date, end_date)

  return(data_frame)
}