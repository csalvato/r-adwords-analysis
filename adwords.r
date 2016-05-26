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

keywords_elog <- bing_keywords_elog

###################################### END CREATE ELOGS ################################################


###################################### CREATE AdWords DATA FRAMES ##############################################
# Note, number of transactions is NOT the same as number of orders
adwords_user_overview <- user_overview(adwords_keywords_elog)

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

adwords_overall_performance_over_time <- AdWordsUtilities::overall_performance_over_time(keywords_elog, plot = TRUE)
                              

adwords_summary_overview <- keywords_elog %>%
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

adwords_order_per_week <- PowerSupplyUtilities::orders_per_week(adwords_keywords_elog, 
                                                                keyword_filter="paleo meals", 
                                                                campaign_filter="Paleo Performers")

###################################### CREATE Bing DATA FRAMES ##############################################
# Note, number of transactions is NOT the same as number of orders
bing_user_overview <- user_overview(bing_keywords_elog)

keywords_campaign_overview <- keywords_elog %>%
  group_by(keyword, campaign_name) %>%
  summarize_bing_elog %>%
  ungroup %>%
  arrange(desc(earnings))

keywords_campaign_device_matchtype_overview <- keywords_elog %>%
  group_by(keyword, campaign_name, device, match_type) %>%
  summarize_bing_elog %>%
  ungroup %>%
  arrange(desc(earnings))


keywords_campaign_matchtype_overview <- keywords_elog %>%
  group_by(keyword, campaign_name, match_type) %>%
  summarize_bing_elog %>%
  ungroup %>%
  arrange(desc(earnings))


keywords_overview <- keywords_elog %>%
  group_by(keyword) %>%
  summarize_bing_elog %>%
  arrange(desc(earnings))

keywords_weekly_conversion_metrics <- keywords_elog %>%
  group_by(keyword, campaign_name, week) %>%
  summarize_bing_elog %>%
  #filter(grepl("Paleo Performers",campaign_name)) %>%
  ungroup %>%
  arrange(keyword, campaign_name, week) %>%
  select(keyword, campaign_name, week, impressions, clicks, num_acquisitions, click_through_rate, conversion_rate, cost_per_click, contribution_per_click)


bing_summary_overview <- keywords_elog %>%
                         summarize_bing_elog


contribution_per_click_overview <- keywords_elog %>% 
  group_by(keyword,campaign_name) %>% 
  summarize_bing_elog %>% 
  filter(cost > 0 & earnings > 0) %>% 
  group_by(keyword,campaign_name) %>% 
  summarize(total_cost = sum(cost),
            total_contribution = sum(contribution),
            total_clicks = sum(clicks), 
            contribution_per_click = total_contribution/total_clicks,
            cpc_bid_for_2x_ROAS = contribution_per_click/2)

bing_orders_per_week <- PowerSupplyUtilities::orders_per_week_by_geo(bing_keywords_elog,
                                                                     keyword_filter="paleo meals")
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
  #filter(grepl("Paleo Performers",campaign_name)) %>%
  group_by(keyword, campaign_name, week) %>%
  summarize(cost = sum(cost, na.rm = TRUE),
            contribution = sum(money_in_the_bank_paid_to_us,na.rm=TRUE) *.25) %>%
  mutate(cum_contribution = cumsum(contribution), 
         cum_cost = cumsum(cost),
         cum_ROI = cum_contribution - cum_cost) %>%
  gather(type,value,cum_cost,cum_contribution,cum_ROI)


bing_overall_performance_over_time <- BingUtilities::overall_performance_over_time(bing_keywords_elog, plot = TRUE)

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

# Plot orders per week by geo
plot( ggplot( num_orders_per_week ) + 
      aes(week,num_orders,group=campaign_name,col=campaign_name,fill=campaign_name) + 
      geom_line() +
      facet_wrap(~campaign_name, ncol=2))



###################################### PALEO MEALS COHORT VIEWS ###################################


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

##add variable with cohort_week number renamed as "cohort_#"
cohort_week2 <- character()
for(i in 1:length(paleo_meals_cohort$week_number)) {
        cohort_week2[i] <- paste0("cohort_",paleo_meals_cohort$cohort_week[i])
}
paleo_meals_cohort <- cbind(paleo_meals_cohort,cohort_week2)


##Organize dataframes and prepare for plotting

##Dataframe of Total Cost, Contribution and ROI for paleo meals cohorts

paleo_meals_cohorts_total_ROI <- paleo_meals_cohort %>%
        group_by(cohort_week) %>%
        summarize(cohort_cost = sum(cost, na.rm = TRUE),
                  cohort_contribution = sum(money_in_the_bank_paid_to_us,na.rm=TRUE) *.25) %>%
        mutate(cohort_ROI = cohort_contribution - cohort_cost) %>%
        gather(type,value,cohort_cost,cohort_contribution,cohort_ROI)

##Set up ROI as a separate variable to allow it to be plotted differently

paleo_meals_cohorts_total_ROI <- as.data.frame(paleo_meals_cohorts_total_ROI)
cohort_ROI <- filter(paleo_meals_cohorts_total_ROI,type == "cohort_ROI")

paleo_meals_cohorts_total_ROI <- paleo_meals_cohorts_total_ROI %>%
                        filter(type != "cohort_ROI") %>%
                        merge(cohort_ROI,by="cohort_week",all.x=TRUE) %>%
                        arrange(cohort_week,desc(type.x))

##make cost values negative
for(i in 1:length(paleo_meals_cohorts_total_ROI$cohort_week)) {
        if(paleo_meals_cohorts_total_ROI[i,"type.x"]=="cohort_cost") {
                paleo_meals_cohorts_total_ROI[i,"value.x"] <- -(paleo_meals_cohorts_total_ROI[i,"value.x"])
        } else {next}
}

##Dataframe of cumulative cost, contribution and ROI over time.
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


###################################### PALEO MEALS COHORT PLOTS ###################################


##Weekly Cohort Plot 1: Stacked barchart of Contribution/Cost per Cohort Week with Overlaid ROI points
##!!Need to figure out how to add "cohort_ROI" line to the legend.

max_cw <- max(paleo_meals_cohort$cohort_week)
ymax <- ceiling(signif(max(paleo_meals_cohorts_total_ROI$value.x,paleo_meals_cohorts_total_ROI$value.y),2))
ymin <- floor(signif(min(paleo_meals_cohorts_total_ROI$value.x,paleo_meals_cohorts_total_ROI$value.y),2))

plot( 
        ggplot(
                paleo_meals_cohorts_total_ROI, 
                aes(x=cohort_week,y=value.x, fill=type.x)) +
                geom_bar(stat="identity", position="identity") +
                geom_line(data=paleo_meals_cohorts_total_ROI,aes(x=cohort_week,y=value.y),linetype=1,size=1,color="grey55") +
                geom_point(data=paleo_meals_cohorts_total_ROI,aes(x=cohort_week,y=value.y,color="cohort_ROI"),shape=21,size=2,fill="chartreuse1",color="turquoise4") +
                scale_x_continuous(breaks=seq(1,max_cw,1)) +
                scale_y_continuous(breaks=seq(ymin,ymax,500),labels=scales::dollar) + 
                scale_fill_manual(values=c("springgreen4","red2")) +
                guides(fill=guide_legend(title=NULL)) +
                labs(title="Contribution, Cost, and ROI by Cohort Week",x="Cohort Week",y="") 
)

##Weekly Cohort Plot 2: Number of weeks it took each cohort to break even

max_be <- max(paleo_meals_cohort_breakeven$weeks_until_breakeven,na.rm=TRUE)
plot(
        ggplot(
                paleo_meals_cohort_breakeven, 
                aes(x=cohort_week,y=weeks_until_breakeven)) +
                geom_bar(stat="identity", position="dodge") +
                scale_x_continuous(breaks=seq(1,max_cw,1)) +
                scale_y_continuous(breaks=seq(0,max_be,1)) +
                labs(title="Weeks to Break Even per Cohort Week",x="Cohort Week",y="Number of Weeks")
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
