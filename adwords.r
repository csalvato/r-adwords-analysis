library(RPostgreSQL)
library(RJDBC)
library(dplyr)
library(tidyr)
library(GetoptLong)

# Set reporting parameters
start_date = '2015-12-17'
end_date = toString(Sys.Date())

# Pull in adwords campaign data and format for use
adwords_data <- read.csv(text=readLines('Campaign Performance Report.csv')[-(1:1)], header=TRUE)
adwords_data <- head(adwords_data, -4)
adwords_data$Day <- as.Date(adwords_data$Day, "%Y-%m-%d") # Change Day to date type for date searching/filtering
adwords_data$Campaign.ID <- as.integer(as.character(adwords_data$Campaign.ID)) # Make sure campaign IDs are integers for future joins
adwords_data$Cost <- as.numeric(as.character(adwords_data$Cost)) # Make sure cost is a float for totaling
adwords_data <- filter(adwords_data, Day >= as.Date(start_date, "%Y-%m-%d"), Day <= as.Date(end_date, "%Y-%m-%d"))

# Create a join table of campaign names and IDs
campaigns_table <- adwords_data %>% 
                      group_by(Campaign.ID)%>% 
                      summarize(Campaign.Name = first(Campaign))

# Retrieve revenue data
pgsql <- JDBC("org.postgresql.Driver", "../database_drivers/postgresql-9.2-1004.jdbc4.jar", "`")
#heroku_db <- dbConnect(pgsql, "jdbc:postgresql://ec2-54-221-203-136.compute-1.amazonaws.com:5502/dfh97e63ls7ag8?user=u1gg5j81iss15&password=p1g2km19noav948l6q7net768vu&ssl=true&sslfactory=org.postgresql.ssl.NonValidatingFactory")
datawarehouse_db <- dbConnect(pgsql, "jdbc:postgresql://127.0.0.1:5438/mps_oltp?user=oltp_reader&password=0Ltpr33@donly")

# Notes from John Wooley:
# This query should give real-time feedback, but could change over the course of the week
# if the users make changes.  This could be run on the Heroku follower if the
# dim_users table was removed.  I included it because there is useful info in it (or will
# be on Friday - those fields are updated after deliveries are finalized).

# Kevin says that a transaction record is created immediately for new orders and
# updated with any changes through to Thursday night.

# Recurring orders are created on Tuesday and updated until Thursday night.

# Transaction values can be a bit confusing, because they include system credits
# and discounts.  The price field is generally == list price of the meal plan, but there
# are situations where it can get nuked for older data.
# query <- qq("
# select
#      du.name
# --    , du.user_id
#     , du.lifetime_transaction_value
#     , du.lifetime_orders
#     , du.first_order_week
#     , dl.store_front_id
#     , gsm.*
# from 
#   stage.gs_mixpanel gsm
# inner join
#   dim_users du on gsm.user_id = du.user_id
# left outer join
#   dim_locations dl on dl.location_id = du.loc_primary
# order by 1")
query <- qq("
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

#********************************************************************************
#TODO: This includes the list price, but does not subtract out a discount 
#      (e.g. first order discount, credit from previous refun applied).  
#      Will need to figure out a way to subtract out discounts they receive 
#      on an order.
#********************************************************************************
db_transactions <- dbGetQuery(datawarehouse_db, query)
View(db_transactions)

# Notes from John: 
# The data from this query will be updated nightly.  
# The numbers for upcoming week will not be solid until Friday morning.

# This will not catch gift cards and will include cancelled orders 
# unless you uncomment the where clause. 

# You can look at the available field and decide what you might want. 
# The price field is list_price, which is not 100% accurate but should  
# be reasonable for recent data.  The transaction_amount with fluctutate some 
# if there are refunds.   

# Week_start is Monday of the week the orders meals would be delivered in.
# It is not the same as the created_at field above, which 
# is a record creation timestamp. 

# query <- "select 
#  * 
# from 
#   fact_orders 
#     inner join 
#   stage.gs_mixpanel vw on fact_orders.user_id = vw.user_id 
#     inner join 
#   dim_users du on fact_orders.user_id = du.user_id 
# -- where 
# --   not fo.cancelled"
# unknown_data <- dbGetQuery(datawarehouse_db, query)
  
# dbDisconnect(heroku_db)
dbDisconnect(datawarehouse_db)

# Make sure campaign IDs are integers for future joins
db_transactions$latest_ad_utm_campaign <- as.integer(db_transactions$latest_ad_utm_campaign)

# Join with Campaign ID lookup table.
db_transactions <- left_join(db_transactions, campaigns_table, by=c(latest_ad_utm_campaign = "Campaign.ID"))

# Create a table showing total value by campaign
grouped_data1 <- db_transactions %>% 
                  group_by(Campaign.Name, latest_ad_device) %>% 
                  summarize(Earnings = sum(money_in_the_bank_paid_to_us))
View(grouped_data1)

grouped_data2 <- db_transactions %>% 
                  group_by(Campaign.Name) %>% 
                  summarize(Earnings = sum(money_in_the_bank_paid_to_us))
View(grouped_data2)

grouped_data3 <- db_transactions %>% 
                  group_by(user_id) %>% 
                  summarize(name = first(user_name), num_transactions=length(transaction_date), earnings = sum(money_in_the_bank_paid_to_us))
View(grouped_data3)

grouped_data4 <- db_transactions %>% 
                  group_by(latest_ad_awkeyword) %>% 
                  summarize(num_users=n_distinct(user_id), num_transactions=length(transaction_date), Earnings = sum(money_in_the_bank_paid_to_us))
View(grouped_data4)

grouped_data5 <- db_transactions %>%
                summarize(earnings=sum(money_in_the_bank_paid_to_us))
View(grouped_data5)
########################