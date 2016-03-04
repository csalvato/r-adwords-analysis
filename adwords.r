library(utils)
library(graphics)
library(RPostgreSQL)
library(RJDBC)
library(plyr)
library(dplyr)
library(tidyr)
library(GetoptLong)
library(gridExtra)
library(lubridate) 

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

# Set reporting parameters
start_date = '2015-12-17'
end_date = toString(Sys.Date())

# Retrieve revenue data
pgsql <- JDBC("org.postgresql.Driver", "../database_drivers/postgresql-9.2-1004.jdbc4.jar", "`")
#heroku_db <- dbConnect(pgsql, "jdbc:postgresql://ec2-54-221-203-136.compute-1.amazonaws.com:5502/dfh97e63ls7ag8?user=u1gg5j81iss15&password=p1g2km19noav948l6q7net768vu&ssl=true&sslfactory=org.postgresql.ssl.NonValidatingFactory")
datawarehouse_db <- dbConnect(pgsql, "jdbc:postgresql://127.0.0.1:5438/mps_oltp?user=oltp_reader&password=0Ltpr33@donly")

purchases_query <- qq("
select
  t.created_at as transaction_date
  , du.name as user_name
  , t.discount_amount
  , ti.credit_used
  , t.sales_tax_amount
  , (CASE t.refunded
  WHEN 't' THEN
  ti.price
  ELSE
  0
  END) as refund_amount
  , (CASE 
  WHEN d.type = 'GiftCard'
  THEN d.amount
  ELSE
  ti.price - t.sales_tax_amount + t.discount_amount
  END) as retail_price
  , (CASE t.refunded
  WHEN 't' THEN
  0
  ELSE
  ti.price - ti.credit_used
  END) as money_in_the_bank_paid_to_us
  , (CASE
  WHEN d.type = 'GiftCard'
  THEN 'Gift Card'
  WHEN ti.recurring = 't'
  THEN 'Recurring Food Purchase'
  ELSE 
  'One-Time Meal Purchase'
  END
  ) as purchase_type
  --, ti.name as meal_plan
  --, ti.recurring
  --  , l.name
  --  , l.city
  --  , l.state
  --  , l.store_front_id
  --  , l.home_delivery
  , mp.*
  from
  transactions t
  inner join
  transaction_items ti on t.id = ti.transaction_id
  inner join
  dim_users du on t.user_id = du.user_id
  inner join
  (
  select distinct on (gsm.user_id)
  gsm.*
  from
  stage.gs_mixpanel gsm
  order by gsm.user_id, mixpanel_event_timestamp
  ) mp on du.user_id = mp.user_id
  --left outer join
  --  locations l on ti.location_id = l.id
  left outer join
  discounts d on ti.discount_id = d.id
where
  t.created_at between '@{start_date}' and '@{end_date}'
--  AND
--  t.refunded IS NOT TRUE
order by t.created_at desc")

adwords_campaigns_query <- qq("
select
  *
from 
  stage.gs_adwords_campaigns awc
where 
  awc.date between '@{start_date}' and '@{end_date}'
order by awc.date desc")

db_adwords_campaigns <- dbGetQuery(datawarehouse_db, adwords_campaigns_query)
db_transactions <- dbGetQuery(datawarehouse_db, purchases_query)

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

# Join Campaign names with db transactions to populate campaign name
keywords_transactions <- db_adwords_keywords %>% 
                          group_by(campaign_id)%>% 
                          summarize(campaign_name = first(campaign_name)) %>%
                          right_join(db_transactions, by=c(campaign_id = "campaign_id"))

# Create keywords elog
keywords_elog <- rbind.fill(keywords_transactions, db_adwords_keywords)
keywords_elog$week <- floor_date(keywords_elog$date, "week") - days(1)

keywords_overview <- keywords_elog %>%
                      group_by(keyword, campaign_name) %>%
                      summarize(cost = sum(cost, na.rm = TRUE),
                                estimated_impressions = floor(sum(impressions/est_search_impression_share, na.rm=TRUE)),
                                #average_position=(impressions*avg_position)
                                impressions = sum(impressions, na.rm = TRUE),
                                est_search_impression_share = impressions/estimated_impressions,
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
                      ungroup() %>%
                      arrange(desc(earnings))
View(keywords_overview)

# Join Campaign names with db transactions to populate campaign name
campaigns_transactions <- db_adwords_campaigns %>% 
                          group_by(campaign_id)%>% 
                          summarize(campaign_name = first(campaign_name)) %>%
                          right_join(db_transactions, by=c(campaign_id = "campaign_id"))

# Create campaigns elog
campaigns_elog <- rbind.fill(campaigns_transactions, db_adwords_campaigns)
# add week start dates
campaigns_elog$week <- floor_date(campaigns_elog$date, "week") - days(1)

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


# Note, number of transactions is NOT the same as number of orders
user_keywords <- keywords_elog %>%
                  group_by(user_id) %>%
                  summarize(keyword = first(keyword))

user_overview <- campaigns_elog %>% 
                  filter(!is.na(user_id)) %>% #Remove NA user_ids (which means they are not monetary transactions)
                  group_by(user_id) %>%
                  summarize(name = first(user_name), 
                            num_transactions=length(transaction_date), 
                            earnings = sum(money_in_the_bank_paid_to_us),
                            campaign_name = first(campaign_name)) %>%
                  left_join(user_keywords, by=c(user_id="user_id"))
View(user_overview)

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