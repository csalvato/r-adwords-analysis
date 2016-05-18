# To install RAdwords, you must install via github
# library(devtools)
# install_github('jburkhardt/RAdwords')
# Can look up metrics info with:
# metrics("KEYWORDS_PERFORMANCE_REPORT")
#install.packages("devtools")
library(devtools)

install("SalvatoUtilities")
install("AdWordsUtilities")
install("MixpanelUtilities")
install("PowerSupplyUtilities")

SalvatoUtilities::detach_all_packages()

library(utils)
library(graphics)
library(RPostgreSQL)
library(RJDBC)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2) 
library(lubridate) 
library(Rmisc)
library(RMixpanel)
library(RAdwords)
library(readr)
library(SalvatoUtilities)
library(AdWordsUtilities)
library(MixpanelUtilities)
library(PowerSupplyUtilities)

# Set reporting parameters
start_date = '2015-12-17, 04:00:00'
#end_date = paste(toString(Sys.Date() - days(0)), "03:59:59") #yesterday
# start_date = paste(toString(Sys.Date() - days(8)), "04:00:00")
end_date = paste(toString(Sys.Date() - days(0)), "03:59:59")
#start_date = '2016-04-28, 04:00:00'
#end_date = '2016-04-28, 03:59:59'

ppc_events <- all_ppc_raw_completed_order_events( from = start_date, to = end_date )
mixpanel_adwords_conversions <- ppc_events[["adwords"]]
mixpanel_adwords_conversions <- clean_adwords_raw_completed_order_events(mixpanel_adwords_conversions)
mixpanel_bing_conversions <- ppc_events[["bing"]]

##### Retrieve AdWords Spend/Click Data
adwords_keywords_data <- keyword_performance_data(from=as.Date(start_date), to=as.Date(end_date))
adwords_campaigns_data <- campaign_performance_data(from=as.Date(start_date), to=as.Date(end_date))

# Retrieve revenue data
db_transactions <- get_transactions_data(from=start_date, to=end_date)
db_influencer_metrics <- get_referrals_data(from=start_date, to=end_date)

heroku_db <- dbConnect(pgsql, string_from_file("jdbc_heroku_string.txt"))
db_first_transactions <- dbGetQuery(heroku_db, GetoptLong::qq(paste("SELECT 
                                                                        * 
                                                                     FROM
                                                                       (select 
                                                                       min(t.created_at) as first_transaction,
                                                                       u.id,
                                                                       u.name
                                                                       from users u
                                                                       inner join
                                                                       transactions t on t.user_id = u.id
                                                                       where
                                                                       u.id IN (", paste(shQuote(db_transactions$app_user_id, type = "sh"), collapse=','),
                                                                                ")
                                                                       group by u.id
                                                                       ORDER BY first_transaction desc) first_transactions
                                                                     WHERE
                                                                     first_transaction between '@{start_date}' and '@{end_date}'")))
heroku_db <- dbDisconnect(heroku_db)

# Join Mixpanel Conversion Data with transaction data
unique_users <- distinct(mixpanel_adwords_conversions, app_user_id)

db_transactions  <- db_transactions %>% inner_join(unique_users, by="app_user_id")

#Filter out people where their first order was not in the specified start_date and end_date
db_transactions <- db_transactions %>% filter(is.element(app_user_id, db_first_transactions$id))

# Format database transactions for future use
db_transactions <- db_transactions %>% rename(device=latest_ad_device, 
                                              campaign_id=latest_ad_utm_campaign,
                                              keyword=latest_ad_awkeyword,
                                              ad_group_id=latest_ad_awadgroupid,
                                              match_type=latest_ad_awmatchtype)
db_transactions$campaign_id <- as.integer(as.character(db_transactions$campaign_id))
db_transactions$date <- as.Date(db_transactions$transaction_date, format="%Y-%m-%d")
db_transactions$day_of_week <- weekdays(as.Date(db_transactions$date,'%Y-%m-%d'))
db_transactions$match_type <- as.match_type(db_transactions$match_type)
db_transactions$keyword <- as.adwords.keyword(db_transactions$keyword)
db_transactions <- db_transactions %>% date_filter(start_date, end_date)
db_transactions <- db_transactions %>% select(-user_id)
db_transactions <- db_transactions %>% mutate(user_id=as.integer(as.character(app_user_id)))

# Add campaign names to db_transactions log
db_transactions <- adwords_campaigns_data %>% 
                     group_by(campaign_id)%>% 
                     summarize(campaign_name = first(campaign_name)) %>%
                     right_join(db_transactions, by=c(campaign_id = "campaign_id"))


###################################### CREATE ELOGS ################################################
# Create keywords elog
keywords_elog <- rbind.fill(db_transactions, adwords_keywords_data)
keywords_elog$week <- as.week(keywords_elog$date)
keywords_elog <- keywords_elog %>% arrange(week)

# Create join table for user_id and the keyword and campaign_name of first purchase
user_first_acquisition_metrics <- keywords_elog %>%
                                  filter(!is.na(user_id)) %>% #Remove NA user_ids (which means they are not monetary transactions)
                                  group_by(user_id) %>%
                                  summarize(keyword = first(keyword),
                                            campaign_name=first(campaign_name),
                                            campaign_id=first(campaign_id),
                                            device=first(device),
                                            match_type=first(match_type))

#Add influencer metrics to the event log
influencer_metrics_with_user_data <- db_influencer_metrics %>%
                                    rename(week=week_start,
                                           user_id=influencer_id) %>%
                                    mutate(week = as.Date(week, format = '%Y-%m-%d')) %>%
                                    inner_join(user_first_acquisition_metrics, by=c(user_id="user_id")) %>% 
                                    filter(week >= start_date, week <= end_date)

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
                        filter(grepl("Paleo Performers",campaign_name)) %>%
                        mutate(est_search_impression_share = ifelse(!is.na(est_search_impression_share) & est_search_impression_share >= 1.0, 1.0, est_search_impression_share)) %>%
                        ungroup %>%
                        arrange(keyword, campaign_name, week) %>%
                        select(keyword, campaign_name, week, est_search_impression_share, impressions, clicks, num_acquisitions, click_through_rate, conversion_rate, cost_per_click, contribution_per_click)

all_keyword_ROAS_over_time <- keywords_elog %>%
                              group_by(week) %>%
                              summarize_adwords_elog %>%
                              mutate(cum_contribution = cumsum(contribution),
                                     cum_cost = cumsum(cost),
                                     cum_ROI = cum_contribution - cum_cost) %>%
                              gather(type,value,cum_cost,cum_contribution, cum_ROI)
                              

summary_overview <- keywords_elog %>%
                    summarize_adwords_elog

contribution_per_click_overview <- keywords_elog %>% 
                                    group_by(keyword,campaign_name) %>% 
                                    summarize_adwords_elog %>% 
                                    filter(cost > 0 & earnings > 0) %>% 
                                    group_by(keyword,campaign_name) %>% 
                                    summarize(total_cost = sum(cost),
                                              total_contribution = sum(contribution),
                                              total_clicks = sum(clicks), 
                                              contribution_per_click = total_contribution/total_clicks,
                                              cpc_bid_for_2x_ROAS = contribution_per_click/2)

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
  filter(grepl("Paleo Performers",campaign_name)) %>%
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
plot(ggplot(keywords_campaigns_over_time %>% filter(keyword == "paleo meals"), aes(week,value,group=type,col=type,fill=type)) + 
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
              filter(keyword == "paleo meals") %>% 
              #filter(keyword %in% keywords_with_earnings$keyword) %>% 
              filter(device == "dt"), 
            aes(week,value,group=type,col=type,fill=type)) + 
       geom_line() + 
       ggtitle("Keyword Trends on Desktop") + 
       facet_wrap(~keyword + device, ncol=3))


#All keywords impression share
plot( 
  ggplot(
    keywords_weekly_conversion_metrics %>% 
      # Filter by a single keyword, and only include the previous 4 weeks of data.
      filter(week >= Sys.Date() - weeks(4), week <= Sys.Date(), keyword == "paleo meals"), 
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
      filter(week >= Sys.Date() - weeks(4), week <= Sys.Date(), keyword == "paleo meals"), 
    aes(x=week, y=click_through_rate, fill=campaign_name)) +
    geom_bar(stat="identity", position="dodge") +
    ggtitle("Weekly CTR by Keyword") +
    # ylim(0, 0.30) +
    facet_wrap(~keyword, ncol=2)
)

#Three plots. All same keyword, one plot per campaign. Impression share.
plot( 
  ggplot(
    keywords_weekly_conversion_metrics %>% 
      # Filter by a single keyword, and only include the previous 4 weeks of data.
      filter(keyword == "paleo meals", week >= Sys.Date() - weeks(4), week <= Sys.Date()), 
    aes(x=week, y=est_search_impression_share)) +
    geom_bar(stat="identity") +
    ggtitle("Weekly Impression Share by Geo") + 
    ylim(0, 1) +
    facet_wrap(~keyword + campaign_name, ncol=2)
)


############################## Write to file ####################################
# write.excel.csv(db_transactions)
# write.excel.csv(campaign_overview)
# write.excel.csv(keywords_elog)
# write.excel.csv(keywords_overview)
# write.excel.csv(keywords_campaign_overview)
# write.excel.csv(keywords_weekly_conversion_metrics)
# write.excel.csv(user_overview)
# write.excel.csv(keywords_campaign_matchtype_overview)
write.excel.csv(keywords_campaign_device_matchtype_overview)
# write.excel.csv(summary_overview)
write.excel.csv(contribution_per_click_overview)