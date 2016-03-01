library(RPostgreSQL)
library(RJDBC)
library(dplyr)
library(tidyr)
library(GetoptLong)

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

# Format AdWords data appropriately
db_adwords_campaigns$campaign_id <- as.integer(db_adwords_campaigns$campaign_id)
db_adwords_campaigns$cost <- db_adwords_campaigns$cost/1000000
db_adwords_campaigns$budget <- db_adwords_campaigns$budget/1000000
db_adwords_campaigns$device <- gsub("Computers", "dt", db_adwords_campaigns$device)
db_adwords_campaigns$device <- gsub("Tablets with full browsers", "dt", db_adwords_campaigns$device)
db_adwords_campaigns$device <- gsub("Mobile devices with full browsers", "mb", db_adwords_campaigns$device)

# Create a join table of campaign names and IDs
campaigns_table <- db_adwords_campaigns %>% 
                  group_by(campaign_id)%>% 
                  summarize(campaign_name = first(campaign_name))

# Make sure campaign IDs are integers for future joins
db_transactions$latest_ad_utm_campaign <- as.integer(db_transactions$latest_ad_utm_campaign)

# Join with Campaign ID lookup table.
db_transactions <- left_join(db_transactions, campaigns_table, by=c(latest_ad_utm_campaign = "campaign_id"))

# Create a table showing total value by campaign and device.
adwords_metrics_by_campaign_device <- db_adwords_campaigns %>%
                                      group_by(campaign_name, device) %>%
                                      summarize(cost = sum(cost),
                                                impressions = sum(impressions),
                                                clicks = sum(clicks),
                                                ctr = clicks/impressions,
                                                cpc = cost/clicks)
View(adwords_metrics_by_campaign_device)

grouped_data1 <- db_transactions %>% 
                  group_by(campaign_name, latest_ad_device) %>% 
                  summarize(earnings = sum(money_in_the_bank_paid_to_us),
                            contribution = sum(money_in_the_bank_paid_to_us) *.25,
                            num_acquisitions = length(unique(user_id))) %>% 
                  left_join(adwords_metrics_by_campaign_device, by=c(campaign_name = "campaign_name",
                                                                      latest_ad_device = "device"))
grouped_data1$epc <- grouped_data1$earnings / grouped_data1$clicks
grouped_data1$contibution_pc <- grouped_data1$contribution / grouped_data1$clicks
grouped_data1$cpa <- grouped_data1$cost / grouped_data1$num_acquisitions
View(grouped_data1)

# Create a table showing total value by campaign
adwords_metrics_by_campaign <- db_adwords_campaigns %>%
  group_by(campaign_name) %>%
  summarize(cost = sum(cost),
            impressions = sum(impressions),
            clicks = sum(clicks),
            ctr = clicks/impressions,
            cpc = cost/clicks)
View(adwords_metrics_by_campaign)

grouped_data2 <- db_transactions %>% 
                  group_by(campaign_name) %>% 
                  summarize(earnings = sum(money_in_the_bank_paid_to_us),
                            contribution = sum(money_in_the_bank_paid_to_us) *.25,
                            num_acquisitions = length(unique(user_id))) %>% 
                  left_join(adwords_metrics_by_campaign, by=c(campaign_name = "campaign_name"))
grouped_data2$epc <- grouped_data2$earnings / grouped_data2$clicks
grouped_data2$contibution2pc <- grouped_data2$contribution / grouped_data2$clicks
grouped_data1$cpa <- grouped_data1$cost / grouped_data1$num_acquisitions
View(grouped_data2)


# Note, number of transactions is NOT the same as number of orders
grouped_data3 <- db_transactions %>% 
                  group_by(user_id) %>% 
                  summarize(name = first(user_name), 
                            num_transactions=length(transaction_date), 
                            earnings = sum(money_in_the_bank_paid_to_us),
                            campaign_name = first(campaign_name))
View(grouped_data3)

grouped_data4 <- db_transactions %>% 
                  group_by(latest_ad_awkeyword) %>% 
                  summarize(num_users=n_distinct(user_id), num_transactions=length(transaction_date), Earnings = sum(money_in_the_bank_paid_to_us))
View(grouped_data4)

grouped_data5 <- db_transactions %>%
                summarize(earnings=sum(money_in_the_bank_paid_to_us))
View(grouped_data5)
########################