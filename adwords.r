library(RPostgreSQL)
library(RJDBC)
library(dplyr)
library(tidyr)

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
query <- "select
    t.created_at
, du.name
-- , du.first_order_week
--  , du.last_order_week
--  , du.lifetime_transaction_value
--  , du.avg_transaction_value
--  , t.amount as current_transaction_amount
, t.refunded
--  , t.discount_amount
, d.type
, d.amount
, ti.name as meal_plan
, ti.price as list_price
, ti.recurring
--  , l.name
--  , l.city
--  , l.state
--  , l.store_front_id
--  , l.home_delivery
, vw.*
from
transactions t 
inner join
vw_stage_mixpanel_orders vw on t.user_id = vw.user_id
inner join
transaction_items ti on t.id = ti.transaction_id
inner join
dim_users du on t.user_id = du.user_id
left outer join
locations l on ti.location_id = l.id
left outer join
discounts d on ti.discount_id = d.id
order by 1 desc"
db_transactions <- dbGetQuery(datawarehouse_db, query)

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

query <- "select 
 * 
from 
  fact_orders 
    inner join 
  vw_stage_mixpanel_orders vw on fact_orders.user_id = vw.user_id 
    inner join 
  dim_users du on fact_orders.user_id = du.user_id 
-- where 
--   not fo.cancelled"
unknown_data <- dbGetQuery(datawarehouse_db, query)
  
# dbDisconnect(heroku_db)
dbDisconnect(datawarehouse_db)

# Create a table showing total value by campaign
grouped_data1 <- db_transactions %>% 
                  group_by(latest_ad_utm_campaign, latest_ad_device) %>% 
                  summarize(Earnings = sum(list_price))
View(grouped_data1)

grouped_data2 <- db_transactions %>% 
                  group_by(latest_ad_utm_campaign) %>% 
                  summarize(Earnings = sum(list_price))
View(grouped_data2)

grouped_data3 <- db_transactions %>% 
                  group_by(user_id) %>% 
                  summarize(name = first(name), num_transactions=length(created_at), earnings = sum(list_price))
View(grouped_data3)

grouped_data4 <- db_transactions %>% 
                  group_by(latest_ad_awkeyword) %>% 
                  summarize(num_transactions=length(created_at), num_users=n_distinct(user_id), Earnings = sum(list_price))
View(grouped_data4)
########################