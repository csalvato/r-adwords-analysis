library(utils)
library(graphics)
library(RPostgreSQL)
library(RJDBC)
library(plyr)
library(dplyr)
library(tidyr)
library(GetoptLong)
library(ggplot2)
library(lubridate) 
library(Rmisc)

MARGIN <- 0.25
AVG_VALUE_PER_ORDER <- 70
AVG_NUM_ORDERS_IN_LIFETIME <- 10

# User defined functions
as.impression_share <- function(impression_share_vector) {
  impression_share_vector <- gsub("< 10%", "1%", impression_share_vector)
  impression_share_vector <- gsub("%", "", impression_share_vector)
  impression_share_vector <- gsub("--", NA, impression_share_vector)
  impression_share_vector <- as.numeric(impression_share_vector)/100
  return(impression_share_vector)
}

# User defined functions
as.lost_impression_share <- function(lost_impression_share_vector) {
  lost_impression_share_vector <- gsub("> 90%", "90%", lost_impression_share_vector)
  lost_impression_share_vector <- gsub("%", "", lost_impression_share_vector)
  lost_impression_share_vector <- gsub("--", NA, lost_impression_share_vector)
  lost_impression_share_vector <- as.numeric(lost_impression_share_vector)/100
  return(lost_impression_share_vector)
}

as.match_type <- function(valuetrak_match_type_vector) {
  valuetrak_match_type_vector <- gsub("b", "Broad", valuetrak_match_type_vector)
  valuetrak_match_type_vector <- gsub("e", "Exact", valuetrak_match_type_vector)
  valuetrak_match_type_vector <- gsub("p", "Phrase", valuetrak_match_type_vector)
  return(valuetrak_match_type_vector)
}

as.money <- function(money_vector){
  return(money_vector/1000000)
}

as.device <- function(device_vector) {
  device_vector <- gsub("Computers", "dt", device_vector)
  device_vector <- gsub("Tablets with full browsers", "dt", device_vector)
  device_vector <- gsub("Mobile devices with full browsers", "mb", device_vector)
  return(device_vector)
}

as.week <- function(date_vector){
  floor_date(date_vector, "week") + days(1)
}

string_from_file <- function(file_name){
  GetoptLong::qq(scan(file_name, character()))
}

summarize_adwords_elog <- function(elog_data_frame){
  return (summarize(elog_data_frame, cost = sum(cost, na.rm = TRUE),
                    average_position = weighted.mean(average_position,impressions, na.rm=TRUE),
                    average_quality_score=mean(quality_score, na.rm=TRUE),
                    estimated_available_impressions = sum(impressions/est_search_impression_share, na.rm=TRUE),
                    impressions = sum(impressions, na.rm = TRUE),
                    # imp share is not wholly accurate because of the way the numbers are reported, but close enough. 
                    # May result in 100%+ when impressions are very low 
                    est_search_impression_share = impressions/estimated_available_impressions,
                    clicks = sum(clicks, na.rm = TRUE),
                    click_through_rate = clicks/impressions,
                    num_acquisitions = n_distinct(user_id, na.rm = TRUE),
                    conversion_rate = num_acquisitions/clicks,
                    cost_per_click = cost/clicks,
                    earnings = sum(money_in_the_bank_paid_to_us, na.rm = TRUE),
                    contribution = earnings * MARGIN,
                    earnings_per_click = earnings/clicks,
                    contribution_per_click= contribution/clicks,
                    cpa = ifelse(num_acquisitions==0, cost, cost/num_acquisitions),
                    referred_users=sum(new_referred_users, na.rm=TRUE),
                    referred_earnings=sum(referred_users_transaction_amount,na.rm=TRUE),
                    estimated_ltv = num_acquisitions*AVG_NUM_ORDERS_IN_LIFETIME*AVG_VALUE_PER_ORDER*MARGIN,
                    estimated_lifetime_ROAS=(estimated_ltv-cost)/cost,
                    total_earnings = earnings + referred_earnings,
                    total_contribution = total_earnings * MARGIN,
                    actual_ROAS = (total_contribution-cost)/cost)
          )
}

write.adwords.csv <- function(data_frame, file){
  if("keyword" %in% colnames(data_frame))
  {
    data_frame = data_frame %>% mutate(keyword = ifelse(grepl("\\+(.+)",keyword),
                                                 paste0(".",keyword),
                                                 keyword))
  }
  
  write.csv( data_frame,
             file=file,
             eol = "\r\n", 
             row.names=FALSE)
}

date_filter <- function(data_frame, start_date, end_date) {
  return(data_frame %>% filter(date >= start_date, date <= end_date))
}

# Set reporting parameters
start_date = '2015-12-17'
end_date = toString(Sys.Date())

# Retrieve revenue data
pgsql <- JDBC("org.postgresql.Driver", "../database_drivers/postgresql-9.2-1004.jdbc4.jar", "`")
#heroku_db <- dbConnect(pgsql, string_from_file("jdbc_heroku_string.txt"))
datawarehouse_db <- dbConnect(pgsql, string_from_file("jdbc_datawarehouse_string.txt"))

transactions_query <- string_from_file("transactions_query.sql")
adwords_campaigns_query <- string_from_file("adwords_campaigns_query.sql")
influencer_metrics_query <- string_from_file("influencer_metrics_query.sql")

db_influencer_metrics <- dbGetQuery(datawarehouse_db, influencer_metrics_query)
db_adwords_campaigns <- dbGetQuery(datawarehouse_db, adwords_campaigns_query)
db_transactions <- dbGetQuery(datawarehouse_db, transactions_query)

dbDisconnect(datawarehouse_db)

# Get Keywords data from CSV
db_adwords_keywords <- read.csv(file="keyword_performance_report.csv",head=TRUE,sep=",")

# Format AdWords keyword data for future use
names(db_adwords_keywords) <- tolower(names(db_adwords_keywords))
db_adwords_keywords <- rename(db_adwords_keywords,date=day,
                                                  day_of_week=day.of.week,
                                                  campaign_id=campaign.id,
                                                  campaign_name=campaign,
                                                  est_search_impression_share=search.impr..share,
                                                  est_search_impression_share_lost_rank=search.lost.is..rank.,
                                                  average_position=avg..position,
                                                  ad_group_id=ad.group.id,
                                                  ad_group_name=ad.group,
                                                  quality_score=quality.score,
                                                  match_type=match.type)
db_adwords_keywords$cost <- as.money(db_adwords_keywords$cost)
db_adwords_keywords$date <- as.Date(db_adwords_keywords$date, format="%Y-%m-%d")
db_adwords_keywords$device <- as.device(db_adwords_keywords$device)
db_adwords_keywords$est_search_impression_share <- as.impression_share(db_adwords_keywords$est_search_impression_share)
db_adwords_keywords$est_search_impression_share_lost_rank <- as.lost_impression_share(db_adwords_keywords$est_search_impression_share_lost_rank)
db_adwords_keywords$keyword <- tolower(db_adwords_keywords$keyword)
db_adwords_keywords <- db_adwords_keywords %>% date_filter(start_date, end_date)

# Format AdWords campaign data for future use
db_adwords_campaigns$campaign_id <- as.integer(db_adwords_campaigns$campaign_id)
db_adwords_campaigns$cost <- as.money(db_adwords_campaigns$cost)
db_adwords_campaigns$budget <- as.money(db_adwords_campaigns$budget)
db_adwords_campaigns$date <- as.Date(db_adwords_campaigns$date, format="%Y-%m-%d")
db_adwords_campaigns$device <- as.device(db_adwords_campaigns$device)
db_adwords_campaigns$est_search_impression_share <- as.impression_share(db_adwords_campaigns$search_impression_share)
db_adwords_campaigns$search_lost_impression_share_budget <- as.lost_impression_share(db_adwords_campaigns$search_lost_impression_share_budget)
db_adwords_campaigns$search_lost_impression_share_rank <- as.lost_impression_share(db_adwords_campaigns$search_lost_impression_share_rank)
db_adwords_campaigns <- db_adwords_campaigns %>% date_filter(start_date, end_date)

# Format database transactions for future use
db_transactions <- rename(db_transactions, device=latest_ad_device, 
                          campaign_id=latest_ad_utm_campaign,
                          keyword=latest_ad_awkeyword,
                          ad_group_id=latest_ad_awadgroupid,
                          match_type=latest_ad_awmatchtype)
db_transactions$campaign_id <- as.integer(db_transactions$campaign_id)
db_transactions$date <- as.Date(db_transactions$transaction_date, format="%Y-%m-%d")
db_transactions$day_of_week <- weekdays(as.Date(db_transactions$date,'%Y-%m-%d'))
db_transactions$match_type <- as.match_type(db_transactions$match_type)
db_transactions <- db_transactions %>% date_filter(start_date, end_date)

# Handles Tag Manager not properly parsing the + in the keyword (by manually inserting it to all entries)
# This is NOT a sustainable solution.
db_transactions$keyword <- ifelse(substr(db_transactions$keyword, 1, 1) == '+', db_transactions$keyword, paste('+',db_transactions$keyword, sep="")) 

# Add campaign names to db_transactions log
db_transactions <- db_adwords_campaigns %>% 
                     group_by(campaign_id)%>% 
                     summarize(campaign_name = first(campaign_name)) %>%
                     right_join(db_transactions, by=c(campaign_id = "campaign_id"))


###################################### CREATE ELOGS ################################################
# Create keywords elog
keywords_elog <- rbind.fill(db_transactions, db_adwords_keywords)
keywords_elog$week <- as.week(keywords_elog$date)
keywords_elog <- keywords_elog %>% arrange(week)

# Create join table for user_id and the keyword and campaign_name of first purchase
user_first_acquisition_metrics <- keywords_elog %>%
                                  filter(!is.na(user_id)) %>% #Remove NA user_ids (which means they are not monetary transactions)
                                  group_by(user_id) %>%
                                  summarize(keyword = first(keyword),
                                            campaign_name=first(campaign_name),
                                            campaign_id=first(campaign_id),
                                            device=first(device))

#Add influencer metrics to the event log
influencer_metrics_with_user_data <- db_influencer_metrics %>%
                                    rename(week=week_start,
                                           user_id=influencer_id) %>%
                                    mutate(week = as.Date(week, format = '%Y-%m-%d')) %>%
                                    inner_join(user_first_acquisition_metrics, by=c(user_id="user_id"))

keywords_elog <- rbind.fill(keywords_elog, influencer_metrics_with_user_data)

###################################### END CREATE ELOGS ################################################


###################################### CREATE DATA FRAMES ##############################################
campaign_overview <- keywords_elog %>%
                      group_by(campaign_name) %>%
                      summarize_adwords_elog %>%
                      arrange(desc(earnings))

campaign_device_overview <- keywords_elog %>%
                            group_by(device, campaign_name) %>%
                            summarize_adwords_elog %>%
                            ungroup() %>% # Required to sort properly after multiple grouping.
                            arrange(desc(earnings))

device_overview <- keywords_elog %>%
                    group_by(device) %>%
                    summarize_adwords_elog%>%
                    arrange(desc(earnings))

# Note, number of transactions is NOT the same as number of orders
user_overview <- keywords_elog %>% 
                  filter(!is.na(user_id)) %>% #Remove NA user_ids (which means they are not monetary transactions)
                  group_by(user_id) %>%
                  summarize(name = first(user_name), 
                            num_transactions=length(transaction_date), 
                            earnings = sum(money_in_the_bank_paid_to_us, na.rm=TRUE),
                            contribution = earnings*.25,
                            campaign_name = first(campaign_name),
                            keyword=first(keyword),
                            referred_users=sum(new_referred_users, na.rm=TRUE),
                            referred_earnings=sum(referred_users_transaction_amount,na.rm=TRUE),
                            referred_contribution = referred_earnings * .25,
                            total_earnings = earnings + referred_earnings,
                            total_contribution = contribution + referred_contribution)

consolidated_user_overview  <- user_overview %>% select(name,
                                                        keyword, 
                                                        campaign_name,
                                                        contribution,
                                                        referred_contribution,
                                                        total_contribution)

keywords_campaign_overview <- keywords_elog %>%
                              group_by(keyword, campaign_name) %>%
                              summarize_adwords_elog %>%
                              ungroup %>%
                              arrange(desc(earnings))

keywords_campaign_device_matchtype_overview <- keywords_elog %>%
                                                group_by(keyword, campaign_name, device, match_type) %>%
                                                summarize_adwords_elog %>%
                                                ungroup %>%
                                                arrange(desc(earnings))


keywords_campaign_matchtype_overview <- keywords_elog %>%
                                        group_by(keyword, campaign_name, match_type) %>%
                                        summarize_adwords_elog %>%
                                        ungroup %>%
                                        arrange(desc(earnings))


keywords_overview <- keywords_elog %>%
                      group_by(keyword) %>%
                      summarize_adwords_elog %>%
                      arrange(desc(earnings))

keywords_weekly_conversion_metrics <- keywords_elog %>%
                        group_by(keyword, campaign_name, week) %>%
                        summarize_adwords_elog %>%
                        filter(grepl("Geo 1|Geo 2|Geo 3",campaign_name)) %>%
                        mutate(est_search_impression_share = ifelse(est_search_impression_share >= 1.0, 0, est_search_impression_share  )) %>%
                        ungroup %>%
                        arrange(keyword, campaign_name, week) %>%
                        select(keyword, campaign_name, week, est_search_impression_share, impressions, clicks, num_acquisitions, click_through_rate, conversion_rate)

all_keyword_ROAS_over_time <- keywords_elog %>%
                              group_by(week) %>%
                              summarize_adwords_elog %>%
                              mutate(cum_contribution = cumsum(contribution),
                                     cum_cost = cumsum(cost),
                                     cum_ROI = cum_contribution - cum_cost) %>%
                              gather(type,value,cum_cost,cum_contribution, cum_ROI)
                              

summary_overview <- keywords_elog %>%
                    summarize_adwords_elog

######################## View data frames ########################
# View(campaign_overview)
# View(campaign_device_overview)
# View(device_overview)
# View(user_overview)
# View(keywords_campaign_overview)
# View(keywords_overview)
# View(keywords_weekly_conversion_metrics)
# View(summary_overview)
# View(keywords_campaign_matchtype_overview)
# View(keywords_campaign_device_matchtype_overview)

######################## Create Plots ######################## 
keywords_with_earnings <- keywords_overview %>% 
  filter(earnings > 0)

devices_over_time <- keywords_elog %>%
                      group_by(keyword,device,week) %>%
                      summarize(cost = sum(cost, na.rm = TRUE),
                                contribution = sum(money_in_the_bank_paid_to_us,na.rm=TRUE) *.25) %>%
                      mutate(cum_contribution = cumsum(contribution), 
                             cum_cost = cumsum(cost),
                             cum_ROI = cum_contribution - cum_cost) %>%
                      gather(type,value,cum_cost,cum_contribution,cum_ROI)

keywords_over_time <- keywords_elog %>%
  filter(keyword %in% keywords_with_earnings$keyword) %>%
  group_by(keyword,week) %>%
  summarize(cost = sum(cost, na.rm = TRUE),
            contribution = sum(money_in_the_bank_paid_to_us,na.rm=TRUE) *.25) %>%
  mutate(cum_contribution = cumsum(contribution), 
         cum_cost = cumsum(cost),
         cum_ROI = cum_contribution - cum_cost) %>%
  gather(type,value,cum_cost,cum_contribution,cum_ROI)

keywords_campaigns_over_time <- keywords_elog %>%
  filter(keyword %in% keywords_with_earnings$keyword) %>%
  filter(grepl("Geo 1|Geo 2|Geo 3",campaign_name)) %>%
  group_by(keyword, campaign_name, week) %>%
  summarize(cost = sum(cost, na.rm = TRUE),
            contribution = sum(money_in_the_bank_paid_to_us,na.rm=TRUE) *.25) %>%
  mutate(cum_contribution = cumsum(contribution), 
         cum_cost = cumsum(cost),
         cum_ROI = cum_contribution - cum_cost) %>%
  gather(type,value,cum_cost,cum_contribution,cum_ROI)

#Overall profits over time
plot(ggplot(all_keyword_ROAS_over_time, 
            aes(week,value,group=type,col=type,fill=type)) + 
     geom_line()
    )

#Profits over time by keyword
plot(ggplot(keywords_over_time, aes(week,value,group=type,col=type,fill=type)) + 
       geom_line() + 
       ggtitle("Keyword Trends") + 
       facet_wrap(~keyword))

#Profits over time by keyword and campaign
plot(ggplot(keywords_campaigns_over_time %>% filter(keyword == "+paleo +meals"), aes(week,value,group=type,col=type,fill=type)) + 
       geom_line() + 
       ggtitle("Keyword Trends by Campaign") + 
       facet_wrap(~keyword + campaign_name, ncol=2))

#Profits over time by keyword and device
plot(ggplot(devices_over_time %>% 
              filter(keyword %in% keywords_with_earnings$keyword) %>% 
              filter(device == "mb"), 
            aes(week,value,group=type,col=type,fill=type)) + 
             geom_line() + 
             ggtitle("Keyword Trends on Mobile") + 
             ylim(-1000,1500) +
             facet_wrap(~keyword + device, ncol=3))

plot(ggplot(devices_over_time %>% 
              filter(keyword %in% keywords_with_earnings$keyword) %>% 
              filter(device == "dt"), 
            aes(week,value,group=type,col=type,fill=type)) + 
       geom_line() + 
       ggtitle("Keyword Trends on Desktop") + 
       ylim(-1000,1500) +
       facet_wrap(~keyword + device, ncol=3))


#All keywords impression share
plot( 
  ggplot(
    keywords_weekly_conversion_metrics %>% 
      # Filter by a single keyword, and only include the previous 4 weeks of data.
      filter(week >= Sys.Date() - weeks(4), week <= Sys.Date()), 
    aes(x=week, y=est_search_impression_share, fill=campaign_name)) +
    geom_bar(stat="identity", position="dodge") +
    ggtitle("Weekly Impression Share by Geo") + 
    ylim(0, 1) +
    facet_wrap(~keyword, ncol=2)
  )

#All keywords Clickthrough Rate
plot( 
  ggplot(
    keywords_weekly_conversion_metrics %>% 
      # Filter by a single keyword, and only include the previous 4 weeks of data.
      filter(week >= Sys.Date() - weeks(4), week <= Sys.Date()), 
    aes(x=week, y=click_through_rate, fill=campaign_name)) +
    geom_bar(stat="identity", position="dodge") +
    ggtitle("Weekly CTR by Keyword") +
    ylim(0, 0.30) +
    facet_wrap(~keyword, ncol=2)
)

#Three plots. All same keyword, one plot per campaign. Impression share.
plot( 
  ggplot(
    keywords_weekly_conversion_metrics %>% 
      # Filter by a single keyword, and only include the previous 4 weeks of data.
      filter(keyword == "+paleo +meals", week >= Sys.Date() - weeks(4), week <= Sys.Date()), 
    aes(x=week, y=est_search_impression_share)) +
    geom_bar(stat="identity") +
    ggtitle("Weekly Impression Share by Geo") + 
    ylim(0, 1) +
    facet_wrap(~keyword + campaign_name, ncol=2)
)


############################## Write to file ####################################
# write.adwords.csv(db_transactions, file ="ad_transactions_and_referrals.csv")
# write.adwords.csv(campaign_overview, file ="campaign_overview.csv")
# write.adwords.csv(keywords_elog, file ="full_keywords_event_log.csv")
# write.adwords.csv(keywords_overview, file ="keywords_overview.csv")
# write.adwords.csv(keywords_campaign_overview, file ="keywords_campaign_overview.csv")
# write.adwords.csv(keywords_weekly_conversion_metrics, file ="keywords_weekly_conversion_metrics.csv")
# write.adwords.csv(user_overview, file ="user_overview.csv")
# write.adwords.csv(keywords_campaign_matchtype_overview, file ="keywords_campaign_matchtype_overview.csv")
write.adwords.csv(keywords_campaign_device_matchtype_overview, file ="keywords_campaign_device_matchtype_overview.csv")
