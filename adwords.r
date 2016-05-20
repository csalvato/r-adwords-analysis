# To install RAdwords, you must install via github
# library(devtools)
# install_github('jburkhardt/RAdwords')
# Can look up metrics info with:
# metrics("KEYWORDS_PERFORMANCE_REPORT")
library(devtools)

install("SalvatoUtilities")
install("AdWordsUtilities")

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
library(SalvatoUtilities)
library(AdWordsUtilities)
library(directlabels)


MARGIN <- 0.25
AVG_VALUE_PER_ORDER <- 70
AVG_NUM_ORDERS_IN_LIFETIME <- 10

MIXPANEL_ACCOUNT <- mixpanelCreateAccount("Power Supply - Main",
                                           token="3fdbf9929d332c37f82380157a564049",
                                           secret="af3b32cc21c7b6e91b71f7c0417735d2", 
                                           key="ce370ab09a166e168d448080b55715f6")

# Set reporting parameters
start_date = '2015-12-17, 04:00:00'
#end_date = paste(toString(Sys.Date() - days(0)), "03:59:59") #yesterday
# start_date = paste(toString(Sys.Date() - days(8)), "04:00:00")
end_date = paste(toString(Sys.Date() - days(1)), "03:59:59")
#start_date = '2016-04-28, 04:00:00'
#end_date = '2016-04-28, 03:59:59'

##### Retrieve Mixpanel AdWords Conversion Data
mixpanel_adwords_conversions <- mixpanelGetEvents(MIXPANEL_ACCOUNT, 
                                                  from = start_date,
                                                  to = end_date,
                                                  event = array("Completed Order"),
                                                  where = '(properties["latest_ad_search"]) and (properties["latest_ad_utm_source"] == "Google")')

##### Retrieve AdWords Spend/Click Data
google_auth <- doAuth()

adwords_keywords_statement <- statement(select=c('Date',
                                                 'DayOfWeek',
                                                 #'HourOfDay',
                                                 'Criteria',
                                                 'Status',
                                                 'CampaignId', 
                                                 'CampaignName',
                                                 'AdGroupId',
                                                 'AdGroupName',
                                                 'Cost',
                                                 'AdNetworkType2', #Network with Search Partners
                                                 'SearchImpressionShare',
                                                 'SearchRankLostImpressionShare',
                                                 'Device',
                                                 'Impressions',
                                                 'Clicks',
                                                 'AveragePosition',
                                                 'QualityScore',
                                                 'PostClickQualityScore',
                                                 'KeywordMatchType'),
                                   report="KEYWORDS_PERFORMANCE_REPORT",
                                   start=format(ymd_hms(start_date), format="%Y%m%d"),
                                   end=format(ymd_hms(end_date), format="%Y%m%d"))

# Make sure to use Adwords Account Id (MCC Id will not work)
adwords_keywords_data <- getData(clientCustomerId="479-107-0932", google_auth=google_auth ,statement=adwords_keywords_statement)

adwords_campaigns_statement <- statement(select=c('Date',
                                                  'DayOfWeek',
                                                  #'HourOfDay',
                                                  'CampaignStatus',
                                                  'CampaignId',
                                                  'CampaignName', 
                                                  'Cost', #Returned in micros (divide by 1,000,000)
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

adwords_campaigns_data <- getData(clientCustomerId="479-107-0932", google_auth=google_auth ,statement=adwords_campaigns_statement)

# Retrieve revenue data
pgsql <- JDBC("org.postgresql.Driver", "database_drivers/postgresql-9.4.1208.jre6.jar", "`")
heroku_db <- dbConnect(pgsql, string_from_file("jdbc_heroku_string.txt"))
datawarehouse_db <- dbConnect(pgsql, string_from_file("jdbc_datawarehouse_string.txt"))

transactions_query <- string_from_file("mixpanel_transactions_query.sql")
influencer_metrics_query <- string_from_file("influencer_metrics_query.sql")

db_influencer_metrics <- dbGetQuery(datawarehouse_db, influencer_metrics_query)
db_transactions <- dbGetQuery(heroku_db, transactions_query)

# Join Mixpanel Conversion Data with Data Warehouse transaction data
mixpanel_adwords_conversions <- data.frame(mixpanel_adwords_conversions)
mixpanel_adwords_conversions  <- mixpanel_adwords_conversions %>% rename(app_user_id = id)
mixpanel_adwords_conversions  <- mixpanel_adwords_conversions %>% mutate(app_user_id = as.numeric(as.character(app_user_id)))
unique_users <- distinct(mixpanel_adwords_conversions, app_user_id)

db_transactions  <- db_transactions %>% inner_join(unique_users, by="app_user_id")

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

dbDisconnect(datawarehouse_db)
dbDisconnect(heroku_db)

#Filter out people where their first order was not in the specified start_date and end_date
db_transactions <- db_transactions %>% filter(is.element(app_user_id, db_first_transactions$id))


# Format AdWords keyword data for future use
# Clean up column names
names(adwords_keywords_data) <- gsub('\\(|\\)',"",tolower(names(adwords_keywords_data)))
# Rename columns, convert values where necessary, 
# and filter everything outside of start_date and end_date
adwords_keywords_data <- adwords_keywords_data %>%
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

# Format AdWords campaign data for future use
names(adwords_campaigns_data) <- gsub('\\(|\\)',"",tolower(names(adwords_campaigns_data)))
adwords_campaigns_data <- adwords_campaigns_data %>%
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

consolidated_user_overview <- user_overview %>% select(name,
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


##########################################################################
######################## Paleo Meals Cohort Views ########################

paleo_meals_cohort <- filter(keywords_elog,keyword == "paleo meals")

##assign ordinal number to weeks
week_number <- numeric()
for(i in 1: length(paleo_meals_cohort$week)) {
        week_number[i] <- which(unique(paleo_meals_cohort$week)==paleo_meals_cohort$week[i])
}
paleo_meals_cohort <- cbind(paleo_meals_cohort,week_number)

##assign each user_id to a 'cohort_week' based on week of first order using the 'paleo meals' keyword
##all future occurrences of that user_id are labelled with the original cohort_week
##if no user_id for a given transaction, cohort_week = week_number

paleo_meals_cohort <- mutate(paleo_meals_cohort,cohort_week = rep(0,length(paleo_meals_cohort$week)))

for(i in 1:length(unique(paleo_meals_cohort$week_number))) {
        users <- paleo_meals_cohort[which(paleo_meals_cohort$week_number==i),"user_id"]
        
        for(k in 1:length(paleo_meals_cohort$user_id)) {
                if(paleo_meals_cohort$cohort_week[k] > 0) {next} 
                if(paleo_meals_cohort$user_id[k] %in% users) {
                        paleo_meals_cohort$cohort_week[k] <- i}
                if(is.na(paleo_meals_cohort$user_id[k]))
                {paleo_meals_cohort$cohort_week[k] <- paleo_meals_cohort$week_number[k]}
        }
}

##add variable with cohort_week number renamed as "cohort_x"
cohort_week2 <- character()
for(i in 1:length(paleo_meals_cohort$week_number)) {
        cohort_week2[i] <- paste0("cohort_",paleo_meals_cohort$cohort_week[i])
}
paleo_meals_cohort <- cbind(paleo_meals_cohort,cohort_week2)


##Organize dataframes and prepare for plotting

##DF of Total Cost, Contribution and ROI

paleo_meals_cohorts_total_ROI <- paleo_meals_cohort %>%
        group_by(cohort_week) %>%
        summarize(cohort_cost = sum(cost, na.rm = TRUE),
                  cohort_contribution = sum(money_in_the_bank_paid_to_us,na.rm=TRUE) *.25) %>%
        mutate(cohort_ROI = cohort_contribution - cohort_cost) %>%
        gather(type,value,cohort_cost,cohort_contribution,cohort_ROI)

##DF of Total Cost, Contribution and ROI with ROI set up as separate variable to allow it to be plotted differently

paleo_meals_cohorts_total_ROI_v2 <- as.data.frame(paleo_meals_cohorts_total_ROI)
cohort_ROI <- filter(paleo_meals_cohorts_total_ROI_v2,type == "cohort_ROI")

paleo_meals_cohorts_total_ROI_v2 <- paleo_meals_cohorts_total_ROI_v2 %>%
                        filter(type != "cohort_ROI") %>%
                        merge(cohort_ROI,by="cohort_week",all.x=TRUE) %>%
                        arrange(cohort_week,desc(type.x))


##DF of cumulative cost, contribution and ROI over time.
paleo_meals_cohorts_over_time <- paleo_meals_cohort %>%
        group_by(cohort_week,week_number) %>%
        summarize(cost = sum(cost, na.rm = TRUE),
                  contribution = sum(money_in_the_bank_paid_to_us,na.rm=TRUE) *.25) %>%
        mutate(cum_contribution = cumsum(contribution), 
               cum_cost = cumsum(cost),
               cum_ROI = cum_contribution - cum_cost) %>%
        gather(type,value,cum_cost,cum_contribution,cum_ROI)

##Create variables to track the ROI breakeven week for a given cohort (if it exists)

paleo_meals_cohorts_over_time <- as.data.frame(paleo_meals_cohorts_over_time)

paleo_meals_cohort_breakeven <- filter(paleo_meals_cohorts_over_time,type=="cum_ROI")

##breaken_week is a logical variable tracking if a Cohort broke even (ROI crossed over zero) in a given week

breakeven_week <- logical()
for(i in 2:length(paleo_meals_cohort_breakeven$week)) {
        if(paleo_meals_cohort_breakeven$value[i] > 0
           & paleo_meals_cohort_breakeven$value[i-1] < 0
           & paleo_meals_cohort_breakeven$cohort_week[i] == paleo_meals_cohort_breakeven$cohort_week[i-1]
              |  paleo_meals_cohort_breakeven$value[i]>0
                 & paleo_meals_cohort_breakeven$cohort_week[i] != paleo_meals_cohort_breakeven$cohort_week[i-1])
                 
          {breakeven_week[i] <- 1} else {breakeven_week[i] <- 0}
        }
        
paleo_meals_cohort_breakeven <- cbind(paleo_meals_cohort_breakeven,breakeven_week)

##create new variable (weeks_unti_breakeven) which gives the number of weeks it took that cohort to break even
cohort_vector <- unique(paleo_meals_cohort_breakeven$cohort_week)
weeks2breakeven <- numeric()

for(i in 1:length(cohort_vector)) {
        bkevn <- paleo_meals_cohort_breakeven[which(paleo_meals_cohort_breakeven$breakeven_week == 1 & paleo_meals_cohort_breakeven$cohort_week == cohort_vector[i]),"week_number"]
        frst <- min(paleo_meals_cohort_breakeven[which(paleo_meals_cohort_breakeven$cohort_week==cohort_vector[i]),"week_number"])
        if(length(bkevn-frst)==0) {
                weeks2breakeven[i] <- NA} else {
                        weeks2breakeven[i] <- ((bkevn - frst)+1)
                } 
        }
weeks_until_breakeven <- numeric()
for(i in 1:length(paleo_meals_cohort_breakeven$cohort_week)) {
        weeks_until_breakeven[i] <- weeks2breakeven[which(paleo_meals_cohort_breakeven$cohort_week[i] == cohort_vector)]
}

paleo_meals_cohort_breakeven <- cbind(paleo_meals_cohort_breakeven,weeks_until_breakeven)


################Paleo Meals Cohort User Retention############
###############################################################
paleo_meals_cohort_users <- paleo_meals_cohort %>%
                                select(campaign_name,app_user_id,user_name,
                                   money_in_the_bank_paid_to_us,purchase_type,
                                   X.city,match_type,device,
                                   week,week_number,cohort_week,cohort_week2) %>%
                                filter(!is.na(user_name))


##For three 'main' campaign geos, want to find the unique number of users in each cohort_week per week
##to get an idea of retention over time as well as magnitude of market.
##Main campaign geos are DC, LA, and SF Paleo Performers

##DC PALEO PERFORMERS

##Filter the cohort_users df by Geo, then attach the number of users per cohort_week for this geo only
paleo_meals_DC_users <- filter(paleo_meals_cohort_users,campaign_name=="DC Paleo Performers")
dc_num_users <- numeric()
dc_cohorts <- unique(paleo_meals_DC_users$cohort_week)
for(i in 1:length(dc_cohorts)) {
                dc_num_users[i] <- length(unique(paleo_meals_DC_users[which(paleo_meals_DC_users$cohort_week==dc_cohorts[i]),"app_user_id"]))
}

dc_num_users_vec <- numeric()
for(i in 1:length(paleo_meals_DC_users$user_name)) {
        dc_num_users_vec[i] <- dc_num_users[which(dc_cohorts==paleo_meals_DC_users$cohort_week[i])]
        
}

paleo_meals_DC_users <- mutate(paleo_meals_DC_users,dc_num_users=dc_num_users_vec)

##Summarizing total contribution by cohort_week and by X.city and including number of users in that cohort.
DC.City_Geo_Cohort_Summary <- paleo_meals_DC_users %>%
        group_by(cohort_week,X.city) %>%
        summarize(total_contribution = sum(money_in_the_bank_paid_to_us,na.rm=TRUE) *.25,
                      users_in_cohort=unique(dc_num_users))

View(DC.City_Geo_Cohort_Summary)

##LA PALEO PERFORMERS

##Filter the cohort_users df by Geo, then attach the number of users per cohort_week for this geo only
paleo_meals_LA_users <- filter(paleo_meals_cohort_users,campaign_name=="LA Paleo Performers")
LA_num_users <- numeric()
LA_cohorts <- unique(paleo_meals_LA_users$cohort_week)
for(i in 1:length(LA_cohorts)) {
        LA_num_users[i] <- length(unique(paleo_meals_LA_users[which(paleo_meals_LA_users$cohort_week==LA_cohorts[i]),"app_user_id"]))
}

LA_num_users_vec <- numeric()
for(i in 1:length(paleo_meals_LA_users$user_name)) {
        LA_num_users_vec[i] <- LA_num_users[which(LA_cohorts==paleo_meals_LA_users$cohort_week[i])]
        
}

paleo_meals_LA_users <- mutate(paleo_meals_LA_users,LA_num_users=LA_num_users_vec)

##Summarizing total contribution by cohort_week and by X.city and including number of users in that cohort.
LA.City_Geo_Cohort_Summary <- paleo_meals_LA_users %>%
        group_by(cohort_week,X.city) %>%
        summarize(total_contribution = sum(money_in_the_bank_paid_to_us,na.rm=TRUE) *.25,
                  users_in_cohort=unique(LA_num_users))

View(LA.City_Geo_Cohort_Summary)


##SF PALEO PERFORMERS

##Filter the cohort_users df by Geo, then attach the number of users per cohort_week for this geo only
paleo_meals_SF_users <- filter(paleo_meals_cohort_users,campaign_name=="SF Paleo Performers")
SF_num_users <- numeric()
SF_cohorts <- unique(paleo_meals_SF_users$cohort_week)
for(i in 1:length(SF_cohorts)) {
        SF_num_users[i] <- length(unique(paleo_meals_SF_users[which(paleo_meals_SF_users$cohort_week==SF_cohorts[i]),"app_user_id"]))
}

SF_num_users_vec <- numeric()
for(i in 1:length(paleo_meals_SF_users$user_name)) {
        SF_num_users_vec[i] <- SF_num_users[which(SF_cohorts==paleo_meals_SF_users$cohort_week[i])]
        
}

paleo_meals_SF_users <- mutate(paleo_meals_SF_users,SF_num_users=SF_num_users_vec)

##Summarizing total contribution by cohort_week and by X.city and including number of users in that cohort.
SF.City_Geo_Cohort_Summary <- paleo_meals_SF_users %>%
        group_by(cohort_week,X.city) %>%
        summarize(total_contribution = sum(money_in_the_bank_paid_to_us,na.rm=TRUE) *.25,
                  users_in_cohort=unique(SF_num_users))

View(SF.City_Geo_Cohort_Summary)
        

################Paleo Meals Cohort Plots - ALL GEOS############
###############################################################

plot( 
        ggplot(
                paleo_meals_cohorts_total_ROI, 
                aes(x=type,y=value, fill=type)) +
                geom_bar(stat="identity", position="dodge") +
                ggtitle("Total ROI for Paleo Meals Cohorts") +
                facet_wrap(~cohort_week, ncol=4)
)

plot( 
        ggplot(
                paleo_meals_cohorts_total_ROI_v2, 
                aes(x=cohort_week,y=value.x, fill=type.x)) +
                geom_bar(stat="identity", position="stack") +
                ggtitle("Costs and Contributions for Paleo Meals Cohorts")
)

##!!Need to figure out how to add "cohort_ROI" line to the legend.

##make cost values negative
for(i in 1:length(paleo_meals_cohorts_total_ROI_v2$cohort_week)) {
        if(paleo_meals_cohorts_total_ROI_v2[i,"type.x"]=="cohort_cost") {
        paleo_meals_cohorts_total_ROI_v2[i,"value.x"] <- -(paleo_meals_cohorts_total_ROI_v2[i,"value.x"])
} else {next}
}

plot( 
        ggplot(
                paleo_meals_cohorts_total_ROI_v2, 
                aes(x=cohort_week,y=value.x, fill=type.x)) +
                geom_bar(stat="identity", position="identity") +
                geom_line(data=paleo_meals_cohorts_total_ROI_v2,aes(x=cohort_week,y=value.y),linetype=1,size=1,color="grey55") +
                geom_point(data=paleo_meals_cohorts_total_ROI_v2,aes(x=cohort_week,y=value.y,color="cohort_ROI"),shape=21,size=2,fill="chartreuse1",color="turquoise4") +
                scale_x_continuous(breaks=seq(1,23,1)) +
                scale_fill_manual(values=c("springgreen4","red2")) +
                guides(fill=guide_legend(title=NULL)) +
                labs(title="Contribution, Cost, and ROI by Cohort Week",x="Cohort Week",y="") 
)


#Cohorts Plot - Profits over time by weekly cohort for keyword "paleo meals"
plot(ggplot(paleo_meals_cohorts_over_time, aes(week,value,group=type,col=type,fill=type)) + 
             geom_line() + 
             ggtitle("Paleo Meals Cohort Analysis") + 
             facet_wrap(~cohort_week, ncol=4))

#Cohorts Plot - Cum ROI over time by weekly cohort for keyword "paleo meals"

##add back variable with cohort_week number renamed as "cohort_x"
cohort_week2 <- character()
for(i in 1:length(paleo_meals_cohort_breakeven$cohort_week)) {
        cohort_week2[i] <- paste0("cohort_",paleo_meals_cohort_breakeven$cohort_week[i])
}
paleo_meals_cohort_breakeven <- cbind(paleo_meals_cohort_breakeven,cohort_week2)

paleo_meals_cohort_breakeven$cohort_week2 <- as.factor(paleo_meals_cohort_breakeven$cohort_week2)

cp <- ggplot(paleo_meals_cohort_breakeven, aes(week_number,value,col=cohort_week2)) + 
        geom_line() + geom_point() + 
        scale_x_continuous(breaks=seq(1,23,1)) +
        labs(title="Cumulative ROI by Cohort",x="Cohort Week",y="") +
        scale_y_continuous(limits = c(-400,1200),breaks =seq(-400,1200,400),labels = scales ::dollar)
ggtitle("Cumulative ROI by Cohort")


direct.label(cp, list(last.points, hjust = .5, vjust = -.5, rot=30))

##plotting number of weeks it took each cohort to break even
 
plot(
        ggplot(
                paleo_meals_cohort_breakeven, 
                aes(x=cohort_week,y=weeks_until_breakeven)) +
                geom_bar(stat="identity", position="dodge") +
                scale_y_continuous(breaks=seq(0,10,1)) +
                labs(title="Weeks to Break Even per Cohort Week",x="Cohort Week",y="Number of Weeks")
)

######################COHORT VIEWS BY GEO##############################

##DF of cumulative cost, contribution and ROI over time BY CAMPAIGN NAME
paleo_meals_cohorts_over_time_geo <- paleo_meals_cohort %>%
        group_by(campaign_name,cohort_week,week_number) %>%
        summarize(cost = sum(cost, na.rm = TRUE),
                  contribution = sum(money_in_the_bank_paid_to_us,na.rm=TRUE) *.25) %>%
        mutate(cum_contribution = cumsum(contribution), 
               cum_cost = cumsum(cost),
               cum_ROI = cum_contribution - cum_cost) %>%
        gather(type,value,cum_cost,cum_contribution,cum_ROI)

##Create variables to track the ROI breakeven week for a given cohort (if it exists)

paleo_meals_cohorts_over_time_geo <- as.data.frame(paleo_meals_cohorts_over_time_geo)

paleo_meals_cohort_breakeven_geo_ROI <- filter(paleo_meals_cohorts_over_time_geo,type=="cum_ROI" & value != 0)
paleo_meals_cohort_breakeven_geo <- filter(paleo_meals_cohorts_over_time_geo,type=="cum_ROI")

##breaken_week is a logical variable tracking if a Cohort broke even (ROI crossed over zero) in a given week
breakeven_week <- logical()
ifelse(paleo_meals_cohort_breakeven_geo$value[1] > 0,breakeven_week[1] <- 1,breakeven_week[1] <- 0)
for(i in 2:length(paleo_meals_cohort_breakeven_geo$week)) {
        if(paleo_meals_cohort_breakeven_geo$value[i] > 0
           & paleo_meals_cohort_breakeven_geo$value[i-1] < 0
           & paleo_meals_cohort_breakeven_geo$cohort_week[i] == paleo_meals_cohort_breakeven_geo$cohort_week[i-1]
           |  paleo_meals_cohort_breakeven_geo$value[i]>0
           & paleo_meals_cohort_breakeven_geo$cohort_week[i] != paleo_meals_cohort_breakeven_geo$cohort_week[i-1])
                
        {breakeven_week[i] <- 1} else {breakeven_week[i] <- 0}
}

paleo_meals_cohort_breakeven_geo <- cbind(paleo_meals_cohort_breakeven_geo,breakeven_week)

##create new variable (weeks_unti_breakeven) which gives the number of weeks it took that cohort to break even
# cohort_vector <- unique(paleo_meals_cohort_breakeven_geo$cohort_week)
# weeks2breakeven <- numeric()
# 
# for(i in 1:length(cohort_vector)) {
#         bkevn <- paleo_meals_cohort_breakeven_geo[which(paleo_meals_cohort_breakeven_geo$breakeven_week == 1 & paleo_meals_cohort_breakeven_geo$cohort_week == cohort_vector[i]),"week_number"]
#         frst <- min(paleo_meals_cohort_breakeven_geo[which(paleo_meals_cohort_breakeven_geo$cohort_week==cohort_vector[i]),"week_number"])
#         if(length(bkevn-frst)==0) {
#                 weeks2breakeven[i] <- NA} else {
#                         weeks2breakeven[i] <- ((bkevn - frst)+1)
#                 } 
# }
# weeks_until_breakeven <- numeric()
# for(i in 1:length(paleo_meals_cohort_breakeven_geo$cohort_week)) {
#         weeks_until_breakeven[i] <- weeks2breakeven[which(paleo_meals_cohort_breakeven_geo$cohort_week[i] == cohort_vector)]
# }
# 
# paleo_meals_cohort_breakeven_geo <- cbind(paleo_meals_cohort_breakeven_geo,weeks_until_breakeven)


################Paleo Meals Cohort Plots ROI BY GEOS############
###############################################################
#Cohort Plot - Profits over time by weekly cohort for keyword "paleo meals"

plot( 
        ggplot(
                paleo_meals_cohorts_total_ROI, 
                aes(x=type,y=value, fill=type)) +
                geom_bar(stat="identity", position="dodge") +
                ggtitle("Total ROI for Paleo Meals Cohorts") +
                facet_wrap(~cohort_week, ncol=4)
)

plot( 
        ggplot(
                paleo_meals_cohorts_total_ROI_v2, 
                aes(x=cohort_week,y=value.x, fill=type.x)) +
                geom_bar(stat="identity", position="stack") +
                ggtitle("Costs and Contributions for Paleo Meals Cohorts")
)

##!!Need to figure out how to add "cohort_ROI" line to the legend.

plot( 
        ggplot(
                paleo_meals_cohorts_total_ROI_v2, 
                aes(x=cohort_week,y=value.x, fill=type.x)) +
                geom_bar(stat="identity", position="stack") +
                geom_line(data=paleo_meals_cohorts_total_ROI_v2,aes(x=cohort_week,y=value.y,color="cohort_ROI"),linetype=1,size=1.25,color="darkslateblue") +
                scale_x_continuous(breaks=seq(1,23,1)) +
                guides(fill=guide_legend(title=NULL)) +
                labs(title="Contribution, Cost, and ROI by Cohort Week",x="Cohort Week",y="") 
)


#Cohorts Plot - Profits over time by weekly cohort for keyword "paleo meals"
# plot(ggplot(paleo_meals_cohorts_over_time_geo, aes(cohort_week,value,group=type,col=type,fill=type)) + 
#              geom_line() + 
#              ggtitle("Paleo Meals Cohort Analysis by Geo") + 
#              facet_wrap(~campaign_name, ncol=4))


#Cohorts Plot - Cum ROI over time by weekly cohort and by geo for keyword "paleo meals"

##add back variable with cohort_week number renamed as "cohort_x"
cohort_week2 <- character()
for(i in 1:length(paleo_meals_cohort_breakeven_geo_ROI$cohort_week)) {
        cohort_week2[i] <- paste0("cohort_",paleo_meals_cohort_breakeven_geo_ROI$cohort_week[i])
}
paleo_meals_cohort_breakeven_geo_ROI <- cbind(paleo_meals_cohort_breakeven_geo_ROI,cohort_week2)

paleo_meals_cohort_breakeven_geo_ROI$cohort_week2 <- as.factor(paleo_meals_cohort_breakeven_geo_ROI$cohort_week2)
paleo_meals_cohort_breakeven_geo_ROI$campaign_name <- as.factor(paleo_meals_cohort_breakeven_geo_ROI$campaign_name)

plot(
        ggplot(paleo_meals_cohort_breakeven_geo_ROI, aes(week_number,value,col=cohort_week2)) + 
             geom_line() + geom_point() + 
             scale_x_continuous(breaks=seq(1,23,5)) +
             labs(title="Cumulative ROI by Cohort and Geo",x="Cohort Week ",y="") +
             scale_y_continuous(limits = c(-400,1200),breaks =seq(-400,1200,400),labels = scales ::dollar) +
             facet_wrap(~campaign_name, ncol=4)
)


#direct.label(cp2, list(last.points, hjust = .5, vjust = -.5, rot=30))

##Cum ROI by Cohort Week - DC Paleo Performers

DC_paleo_ROI_cohorts <- filter(paleo_meals_cohort_breakeven_geo_ROI,campaign_name=="DC Paleo Performers")

dc <- ggplot(DC_paleo_ROI_cohorts, aes(week_number,value,col=cohort_week2)) + 
                geom_line() + geom_point() + 
                scale_x_continuous(breaks=seq(1,23,1)) +
                labs(title="DC Paleo Performers-Cum ROI by Cohort",x="Cohort Week",y="") +
                scale_y_continuous(limits = c(-400,1200),breaks =seq(-400,1200,400),labels = scales ::dollar)

direct.label(dc, list(last.points, hjust = .5, vjust = -.5, rot=30))

##Cum ROI by Cohort Week - LA Paleo Performers

LA_paleo_ROI_cohorts <- filter(paleo_meals_cohort_breakeven_geo_ROI,campaign_name=="LA Paleo Performers")

dc <- ggplot(LA_paleo_ROI_cohorts, aes(week_number,value,col=cohort_week2)) + 
        geom_line() + geom_point() + 
        scale_x_continuous(breaks=seq(1,23,1)) +
        labs(title="LA Paleo Performers-Cum ROI by Cohort",x="Cohort Week",y="") +
        scale_y_continuous(limits = c(-400,1200),breaks =seq(-400,1200,400),labels = scales ::dollar)

direct.label(dc, list(last.points, hjust = .5, vjust = -.5, rot=30))

##Cum ROI by Cohort Week - SF Paleo Performers

SF_paleo_ROI_cohorts <- filter(paleo_meals_cohort_breakeven_geo_ROI,campaign_name=="SF Paleo Performers")

dc <- ggplot(SF_paleo_ROI_cohorts, aes(week_number,value,col=cohort_week2)) + 
        geom_line() + geom_point() + 
        scale_x_continuous(breaks=seq(1,23,1)) +
        labs(title="SF Paleo Performers-Cum ROI by Cohort",x="Cohort Week",y="") +
        scale_y_continuous(limits = c(-400,1200),breaks =seq(-400,1200,400),labels = scales ::dollar)

direct.label(dc, list(last.points, hjust = .5, vjust = -.5, rot=30))

######################END OF COHORT VIEWS##############################

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
