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

# User defined functions
as.impression_share <- function(impression_share_vector) {
  impression_share_vector <- gsub("< ", "", impression_share_vector)
  impression_share_vector <- gsub("> ", "", impression_share_vector)
  impression_share_vector <- gsub("%", "", impression_share_vector)
  impression_share_vector <- gsub("--", NA, impression_share_vector)
  impression_share_vector <- as.numeric(impression_share_vector)/100
  return(impression_share_vector)
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
                    estimated_impressions = floor(sum(impressions/est_search_impression_share, na.rm=TRUE)),
                    #average_position=(impressions*avg_position)
                    impressions = sum(impressions, na.rm = TRUE),
                    est_search_impression_share = impressions/estimated_impressions,
                    clicks = sum(clicks, na.rm = TRUE),
                    click_through_rate = clicks/impressions,
                    num_acquisitions = n_distinct(user_id, na.rm = TRUE),
                    conversion_rate = num_acquisitions/clicks,
                    cost_per_click = cost/clicks,
                    earnings = sum(money_in_the_bank_paid_to_us, na.rm = TRUE),
                    contribution = earnings *.25,
                    earnings_per_click = earnings/clicks,
                    contribution_per_click= contribution/clicks,
                    cpa = ifelse(num_acquisitions==0, cost, cost/num_acquisitions),
                    ROAS = (contribution-cost)/cost,
                    referred_users=sum(new_referred_users, na.rm=TRUE),
                    referred_earnings=sum(referred_users_transaction_amount,na.rm=TRUE))
          
  )
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
                                                  avg_position=avg..position)
db_adwords_keywords$cost <- as.money(db_adwords_keywords$cost)
db_adwords_keywords$date <- as.Date(db_adwords_keywords$date, format="%Y-%m-%d")
db_adwords_keywords$device <- as.device(db_adwords_keywords$device)
db_adwords_keywords$est_search_impression_share <- as.impression_share(db_adwords_keywords$est_search_impression_share)
db_adwords_keywords$est_search_impression_share_lost_rank <- as.impression_share(db_adwords_keywords$est_search_impression_share_lost_rank)
db_adwords_keywords$keyword <- tolower(db_adwords_keywords$keyword)


# Format AdWords campaign data for future use
db_adwords_campaigns$campaign_id <- as.integer(db_adwords_campaigns$campaign_id)
db_adwords_campaigns$cost <- as.money(db_adwords_campaigns$cost)
db_adwords_campaigns$budget <- as.money(db_adwords_campaigns$budget)
db_adwords_campaigns$date <- as.Date(db_adwords_campaigns$date, format="%Y-%m-%d")
db_adwords_campaigns$device <- as.device(db_adwords_campaigns$device)
db_adwords_campaigns$est_search_impression_share <- as.impression_share(db_adwords_campaigns$search_impression_share)


# Format database transactions for future use
db_transactions$latest_ad_utm_campaign <- as.integer(db_transactions$latest_ad_utm_campaign)
db_transactions$date <- as.Date(db_transactions$transaction_date, format="%Y-%m-%d")
db_transactions$day_of_week <- weekdays(as.Date(db_transactions$date,'%Y-%m-%d'))
db_transactions <- rename(db_transactions, device=latest_ad_device, 
                                           campaign_id=latest_ad_utm_campaign,
                                           keyword=latest_ad_awkeyword)

# Handles Tag Manager not properly parsing the + in the keyword (by manually inserting it to all entries)
# This is NOT a sustainable solution.
db_transactions$keyword <- ifelse(substr(db_transactions$keyword, 1, 1) == '+', db_transactions$keyword, paste('+',db_transactions$keyword, sep="")) 

# Add campaign names to db_transactions log
db_transactions <- db_adwords_campaigns %>% 
                     group_by(campaign_id)%>% 
                     summarize(campaign_name = first(campaign_name)) %>%
                     right_join(db_transactions, by=c(campaign_id = "campaign_id"))


###################################### CREATE ELOGS ################################################
# Create campaigns elog
campaigns_elog <- rbind.fill(db_transactions, db_adwords_campaigns)
campaigns_elog <- campaigns_elog %>% arrange(date)
campaigns_elog$week <- as.week(campaigns_elog$date)

# Create keywords elog
keywords_elog <- rbind.fill(db_transactions, db_adwords_keywords)
keywords_elog <- keywords_elog %>% arrange(date)
keywords_elog$week <- as.week(keywords_elog$date)

# Create join table for user_id and the keyword and campaign_name of first purchase
user_first_acquisition_metrics <- keywords_elog %>%
  group_by(user_id) %>%
  summarize(keyword = first(keyword),
            campaign_name=first(campaign_name),
            campaign_id=first(campaign_id))

#Add influencer metrics to the event log
influencer_metrics_with_user_data <- db_influencer_metrics %>%
                                    rename(week=week_start,
                                           user_id=influencer_id) %>%
                                    mutate(week = as.Date(week, format = '%Y-%m-%d')) %>%
                                    inner_join(user_first_acquisition_metrics, by=c(user_id="user_id"))

keywords_elog <- rbind.fill(keywords_elog, influencer_metrics_with_user_data)
campaigns_elog <- rbind.fill(campaigns_elog, influencer_metrics_with_user_data)

###################################### END CREATE ELOGS ################################################

campaign_overview <- campaigns_elog %>%
                      group_by(campaign_name) %>%
                      summarize(cost = sum(cost, na.rm = TRUE),
                                impressions = sum(impressions, na.rm = TRUE),
                                clicks = sum(clicks, na.rm = TRUE),
                                click_through_rate = clicks/impressions,
                                cost_per_click = cost/clicks,
                                earnings = sum(money_in_the_bank_paid_to_us, na.rm = TRUE),
                                contribution = earnings *.25,
                                num_acquisitions = n_distinct(user_id, na.rm = TRUE),
                                earnings_per_click = earnings/clicks,
                                contribution_per_click= contribution/clicks,
                                cpa = ifelse(num_acquisitions==0, cost, cost/num_acquisitions),
                                ROAS = (contribution-cost)/cost) %>%
                      arrange(desc(earnings))
View(campaign_overview)

campaign_device_overview <- campaigns_elog %>%
                            group_by(device, campaign_name) %>%
                            summarize(cost = sum(cost, na.rm = TRUE),
                                      impressions = sum(impressions, na.rm = TRUE),
                                      clicks = sum(clicks, na.rm = TRUE),
                                      click_through_rate = clicks/impressions,
                                      cost_per_click = cost/clicks,
                                      earnings = sum(money_in_the_bank_paid_to_us, na.rm = TRUE),
                                      contribution = earnings *.25,
                                      num_acquisitions = n_distinct(user_id, na.rm = TRUE),
                                      earnings_per_click = earnings/clicks,
                                      contribution_per_click= contribution/clicks,
                                      cpa = ifelse(num_acquisitions==0, cost, cost/num_acquisitions),
                                      ROAS = (contribution-cost)/cost) %>%
                            ungroup() %>% # Required to sort properly after multiple grouping.
                            arrange(desc(earnings))
View(campaign_device_overview)

device_overview <- campaigns_elog %>%
                    group_by(device) %>%
                    summarize(cost = sum(cost, na.rm = TRUE),
                              impressions = sum(impressions, na.rm = TRUE),
                              clicks = sum(clicks, na.rm = TRUE),
                              click_through_rate = clicks/impressions,
                              cost_per_click = cost/clicks,
                              earnings = sum(money_in_the_bank_paid_to_us, na.rm = TRUE),
                              contribution = earnings *.25,
                              num_acquisitions = n_distinct(user_id, na.rm = TRUE),
                              earnings_per_click = earnings/clicks,
                              contribution_per_click= contribution/clicks,
                              cpa = ifelse(num_acquisitions==0, cost, cost/num_acquisitions),
                              ROAS = (contribution-cost)/cost) %>%
                    arrange(desc(earnings))
View(device_overview)

# Pull in friend referral data
user_influencer_metrics <- db_influencer_metrics %>%
                            group_by(influencer_id) %>%
                            summarize(referred_users=sum(new_referred_users, na.rm=TRUE),
                                      referred_earnings=sum(referred_users_transaction_amount,na.rm=TRUE))

# Note, number of transactions is NOT the same as number of orders
user_overview <- campaigns_elog %>% 
                  filter(!is.na(user_id)) %>% #Remove NA user_ids (which means they are not monetary transactions)
                  group_by(user_id) %>%
                  summarize(name = first(user_name), 
                            num_transactions=length(transaction_date), 
                            earnings = sum(money_in_the_bank_paid_to_us),
                            contribution = earnings*.25,
                            campaign_name = first(campaign_name)) %>%
                  left_join(user_keywords, by=c(user_id="user_id")) %>%
                  left_join(user_influencer_metrics, by=c(user_id="influencer_id")) %>%
                  replace(is.na(.), 0)
user_overview$referred_contribution = user_overview$referred_earnings * .25
user_overview$total_earnings = user_overview$earnings + user_overview$referred_earnings
user_overview$total_contribution = user_overview$contribution + user_overview$referred_contribution
View(user_overview)

consolidated_user_overview  <- select(user_overview, name,
                                                     keyword, 
                                                     campaign_name,
                                                     contribution,
                                                     referred_contribution,
                                                     total_contribution)

keywords_campaign_overview <- keywords_elog %>%
                              group_by(keyword, campaign_name) %>%
                              summarize(cost = sum(cost, na.rm = TRUE),
                                        estimated_impressions = floor(sum(impressions/est_search_impression_share, na.rm=TRUE)),
                                        #average_position=(impressions*avg_position)
                                        impressions = sum(impressions, na.rm = TRUE),
                                        est_search_impression_share = impressions/estimated_impressions,
                                        clicks = sum(clicks, na.rm = TRUE),
                                        click_through_rate = clicks/impressions,
                                        num_acquisitions = n_distinct(user_id, na.rm = TRUE),
                                        conversion_rate = num_acquisitions/clicks,
                                        cost_per_click = cost/clicks,
                                        earnings = sum(money_in_the_bank_paid_to_us, na.rm = TRUE),
                                        contribution = earnings *.25,
                                        earnings_per_click = earnings/clicks,
                                        contribution_per_click= contribution/clicks,
                                        cpa = ifelse(num_acquisitions==0, cost, cost/num_acquisitions),
                                        ROAS = (contribution-cost)/cost,
                                        referred_users=sum(new_referred_users, na.rm=TRUE),
                                        referred_earnings=sum(referred_users_transaction_amount,na.rm=TRUE)) %>%
                              ungroup() %>%
                              arrange(desc(earnings))
View(keywords_campaign_overview)

keywords_overview <- keywords_elog %>%
                      group_by(keyword) %>%
                      summarize(cost = sum(cost, na.rm = TRUE),
                                estimated_impressions = floor(sum(impressions/est_search_impression_share, na.rm=TRUE)),
                                #average_position=(impressions*avg_position)
                                impressions = sum(impressions, na.rm = TRUE),
                                est_search_impression_share = impressions/estimated_impressions,
                                clicks = sum(clicks, na.rm = TRUE),
                                click_through_rate = clicks/impressions,
                                num_acquisitions = n_distinct(user_id, na.rm = TRUE),
                                conversion_rate = num_acquisitions/clicks,
                                cost_per_click = cost/clicks,
                                earnings = sum(money_in_the_bank_paid_to_us, na.rm = TRUE),
                                contribution = earnings *.25,
                                earnings_per_click = earnings/clicks,
                                contribution_per_click= contribution/clicks,
                                cpa = ifelse(num_acquisitions==0, cost, cost/num_acquisitions),
                                ROAS = (contribution-cost)/cost,
                                referred_users=sum(new_referred_users, na.rm=TRUE),
                                referred_earnings=sum(referred_users_transaction_amount,na.rm=TRUE)) %>%
                      ungroup() %>%
                      arrange(desc(earnings))
View(keywords_overview)


# Plot all Keywords over time
all_keyword_ROAS_over_time <- keywords_elog %>%
                                group_by(week) %>%
                                summarize(cost= sum(cost, na.rm = TRUE),
                                          contribution = sum(money_in_the_bank_paid_to_us,na.rm=TRUE) *.25) %>%
                                mutate(cum_contribution = cumsum(contribution),
                                       cum_cost = cumsum(cost))

keywords_overview_plot <- ggplot(gather(all_keyword_ROAS_over_time,type,value,cum_cost,cum_contribution), 
                                 aes(week,value,group=type,col=type,fill=type)) + 
                          geom_line()
plot(keywords_overview_plot)

keywords_with_earnings <- keywords_overview %>% 
                          filter(earnings > 0)
keywords_over_time <- keywords_elog %>%
                      filter(keyword %in% keywords_with_earnings$keyword) %>%
                      group_by(keyword,week) %>%
                      summarize(cost= sum(cost, na.rm = TRUE),
                                contribution = sum(money_in_the_bank_paid_to_us,na.rm=TRUE) *.25) %>%
                      mutate(cum_contribution = cumsum(contribution), cum_cost = cumsum(cost))

keywords_over_time <- gather(keywords_over_time,type,value,cum_cost,cum_contribution)

plot(ggplot(keywords_over_time, aes(week,value,group=type,col=type,fill=type)) + 
       geom_line() + 
       ggtitle("Keyword Trends") + 
       facet_wrap(~keyword))

summary_overview <- campaigns_elog %>%
                    summarize(cost = sum(cost, na.rm=TRUE),
                              earnings=sum(money_in_the_bank_paid_to_us, na.rm=TRUE),
                              contribution = earnings *.25,
                              ROAS_to_date = (contribution-cost)/cost,
                              num_acquisitions=n_distinct(user_id, na.rm = TRUE),
                              estimated_ltv = num_acquisitions*10*70*.25,
                              estimated_lifetime_ROAS=(estimated_ltv-cost)/cost)
View(summary_overview)
########################