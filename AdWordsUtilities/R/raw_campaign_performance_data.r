#' Get Raw Campaign Performance Data
#'
#' This is a utility function for that pulls a Campaign Performance Report
#' with the data needed for our analysis. If a field is missing that is 
#' supported by the \code{campaign performance report}, it should be added to
#' this function.
#' NOTE: Requires you to enter Google Authentication info or have
#' .google.auth.RData in your working directory.
#' @param from Date object for start date in format <yyyymmdd>. Inclusive.  If left blank, defaults to Sys.Date()
#' @param to Date object for end date in format <yyyymmdd>. Inclusive. If left blank, defaults to Sys.Date()
#' @return A data frame with the campaign performance data in the timeframe.
#' @export
#' @examples
#' campaign_performance_data(from=20151216, to=20151219)
raw_campaign_performance_data <- function(from=Sys.Date(), to=Sys.Date()) {
  require(RAdwords)
  
  google_auth <- doAuth()

  statement <- statement(select=c('Date',
                                  'DayOfWeek',
                                  # 'HourOfDay',
                                  'CampaignStatus',
                                  'CampaignId',
                                  'CampaignName', 
                                  'Cost', # Returned in micros (divide by 1,000,000)
                                  'AdNetworkType2', #Network with Search Partners
                                  'SearchImpressionShare',
                                  'SearchRankLostImpressionShare',
                                  'SearchBudgetLostImpressionShare',
                                  'Device',
                                  'Amount', #Budget - returned in micros (divide by 1,000,000)
                                  'Impressions',
                                  'Clicks',
                                  'AveragePosition'),
                        report="CAMPAIGN_PERFORMANCE_REPORT",
                        start=format(ymd_hms(start_date), format="%Y%m%d"),
                        end=format(ymd_hms(end_date), format="%Y%m%d"))

  # Make sure to use Adwords Account Id (MCC Id will not work)
  require(yaml)
  require(readr)
  credentials <- yaml.load(read_file("adwords_credentials.yml"))

  data <- getData(clientCustomerId=credentials[['ClientCustomerID']], google_auth=google_auth ,statement=statement)

  return(data)

}