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
install("BingUtilities")

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
library(BingUtilities)

# Set reporting parameters
from = start_date = '2015-12-17 04:00:00'
#end_date = paste(toString(Sys.Date() - days(0)), "03:59:59") #yesterday
# start_date = paste(toString(Sys.Date() - days(8)), "04:00:00")
to = end_date = paste(toString(Sys.Date() - days(0)), "03:59:59")
#start_date = '2016-04-28, 04:00:00'
#end_date = '2016-04-28, 03:59:59'

adwords_keywords_elog <- create_adwords_event_log(from=start_date, to=end_date)
bing_keywords_elog <- create_bing_event_log(from=start_date, to=end_date)

###################################### END CREATE ELOGS ################################################


###################################### CREATE AdWords Visualizations and Reports ##############################################
PowerSupplyUtilities::search_engine_marketing_report(adwords_keywords_elog)

###################################### CREATE Bing DATA FRAMES ##############################################
# Note, number of transactions is NOT the same as number of orders
bing_user_overview <- user_overview(bing_keywords_elog)

BingUtilities::keywords_campaign_device_matchtype_report(bing_keywords_elog)

keywords_weekly_conversion_metrics <- bing_keywords_elog %>%
  group_by(keyword, campaign_name, week) %>%
  BingUtilities::summarize_elog() %>%
  #filter(grepl("Paleo Performers",campaign_name)) %>%
  ungroup %>%
  arrange(keyword, campaign_name, week) %>%
  select(keyword, campaign_name, week, impressions, clicks, num_acquisitions, click_through_rate, conversion_rate, cost_per_click, contribution_per_click)
  #All keywords impression share

#All keywords Clickthrough Rate
plot( 
  ggplot(
    keywords_weekly_conversion_metrics %>% 
      # Filter by a single keyword, and only include the previous 4 weeks of data.
      filter(week >= Sys.Date() - weeks(4), week <= Sys.Date(), keyword == "paleo meals"), 
    aes(x=week, y=click_through_rate, fill=campaign_name)) +
    geom_bar(stat="identity", position="dodge") +
    ggtitle("Bing - Weekly CTR by Keyword") +
    # ylim(0, 0.30) +
    facet_wrap(~keyword, ncol=2)
)

bing_summary_overview <- bing_keywords_elog %>%
                         BingUtilities::summarize_elog()

bing_overall_performance_over_time <- BingUtilities::overall_performance_over_time(bing_keywords_elog, plot = TRUE)

BingUtilities::contribution_per_click_report(bing_keywords_elog)

bing_orders_per_week <- PowerSupplyUtilities::orders_per_week(bing_keywords_elog,
                                                               keyword_filter="paleo meals")
paleo_cohort_views(bing_keywords_elog)

bing_customers_per_month_report <- PowerSupplyUtilities::customers_per_month_report(bing_keywords_elog, file="bing_customers_per_month.csv")

total_customers_per_month_report <- merge(adwords_customers_per_month_report, 
                                          bing_customers_per_month_report, 
                                          by="first_transaction_month", 
                                          all=TRUE, 
                                          suffixes=c(".adwords",".bing")) %>% 
                                      mutate_each(funs(ifelse(is.na(.),0,.))) %>% 
                                      mutate(total_acquisitions=num_acquisitions.adwords+num_acquisitions.bing)
write.excel.csv(total_customers_per_month_report)

keywords_overview <- bing_keywords_elog %>%
  group_by(keyword) %>%
  BingUtilities::summarize_elog()

keywords_with_earnings <- keywords_overview %>% 
  filter(earnings > 0)

devices_over_time <- bing_keywords_elog %>%
                      group_by(keyword,device,week) %>%
                      summarize(cost = sum(cost, na.rm = TRUE),
                                contribution = sum(money_in_the_bank_paid_to_us,na.rm=TRUE) *.25) %>%
                      mutate(cum_contribution = cumsum(contribution), 
                             cum_cost = cumsum(cost),
                             cum_ROI = cum_contribution - cum_cost) %>%
                      gather(type,value,cum_cost,cum_contribution,cum_ROI)

#Profits over time by keyword and device
plot(ggplot(devices_over_time %>% 
              filter(keyword %in% keywords_with_earnings$keyword) %>% 
              filter(device == "mb"), 
            aes(week,value,group=type,col=type,fill=type)) + 
             geom_line() + 
             ggtitle("Bing - Keyword Trends on Mobile") + 
             ylim(-1000,1500) +
             facet_wrap(~keyword + device, ncol=3))

plot(ggplot(devices_over_time %>% 
              filter(keyword == "paleo meals") %>% 
              #filter(keyword %in% keywords_with_earnings$keyword) %>% 
              filter(device == "dt"), 
            aes(week,value,group=type,col=type,fill=type)) + 
       geom_line() + 
       ggtitle("Bing - Keyword Trends on Desktop") + 
       facet_wrap(~keyword + device, ncol=3))



keywords_over_time <- bing_keywords_elog %>%
  filter(keyword %in% keywords_with_earnings$keyword) %>%
  group_by(keyword,week) %>%
  summarize(cost = sum(cost, na.rm = TRUE),
            contribution = sum(money_in_the_bank_paid_to_us,na.rm=TRUE) *.25) %>%
  mutate(cum_contribution = cumsum(contribution), 
         cum_cost = cumsum(cost),
         cum_ROI = cum_contribution - cum_cost) %>%
  gather(type,value,cum_cost,cum_contribution,cum_ROI)

#Profits over time by keyword
plot(ggplot(keywords_over_time, aes(week,value,group=type,col=type,fill=type)) + 
       geom_line() + 
       ggtitle("Bing - Keyword Trends") + 
       facet_wrap(~keyword, ncol=2))

keywords_campaigns_over_time <- bing_keywords_elog %>%
  filter(keyword %in% keywords_with_earnings$keyword) %>%
  #filter(grepl("Paleo Performers",campaign_name)) %>%
  group_by(keyword, campaign_name, week) %>%
  summarize(cost = sum(cost, na.rm = TRUE),
            contribution = sum(money_in_the_bank_paid_to_us,na.rm=TRUE) *.25) %>%
  mutate(cum_contribution = cumsum(contribution), 
         cum_cost = cumsum(cost),
         cum_ROI = cum_contribution - cum_cost) %>%
  gather(type,value,cum_cost,cum_contribution,cum_ROI)

#Profits over time by keyword and campaign
plot(ggplot(keywords_campaigns_over_time %>% filter(keyword == "paleo meals"), aes(week,value,group=type,col=type,fill=type)) + 
       geom_line() + 
       ggtitle("Bing - Keyword Trends by Campaign") + 
       facet_wrap(~keyword + campaign_name, ncol=2))

