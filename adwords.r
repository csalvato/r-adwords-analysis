library(RPostgreSQL)
library(RJDBC)

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
, du.user_id
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

########################

colnames(db_transactions) <- c("date", "sales", "cust")

db_transactions$date <- as.Date(db_transactions$date, "%Y-%m-%d %H:%M:%OS");

elog <- rbind(db_transactions, paypal)

# Remove the 5-character obfuscation from the end of the email address that is placed when an account goes inactive.
elog$cust <- gsub("(^[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,})([A-Z0-9]{5})$", replacement="\\1", elog$cust, ignore.case=TRUE)

elog <- dc.MergeTransactionsOnSameDate(elog);

# Total period of transactions is ~135 weeks.
# Using Calibration period as 68 weeks.  
# 68 weeks from start date (2013-04-25) is 2014-08-14
# Dates calculated using http://www.timeanddate.com/date/dateadded.html
end.of.cal.period <- as.Date("2014-08-14")

elog.cal <- elog[which(elog$date <= end.of.cal.period), ]

split.data <- dc.SplitUpElogForRepeatTrans(elog.cal);
clean.elog <- split.data$repeat.trans.elog;

freq.cbt <- dc.CreateFreqCBT(clean.elog);

tot.cbt <- dc.CreateFreqCBT(elog.cal)
cal.cbt <- dc.MergeCustomers(tot.cbt, freq.cbt)

birth.periods <- split.data$cust.data$birth.per
last.dates <- split.data$cust.data$last.date
cal.cbs.dates <- data.frame(birth.periods, last.dates, end.of.cal.period)
cal.cbs <- dc.BuildCBSFromCBTAndDates(cal.cbt, cal.cbs.dates, per="week")

params <- pnbd.EstimateParameters(cal.cbs);
LL <- bgnbd.cbs.LL(params, cal.cbs);

p.matrix <- c(params, LL);

for (i in 1:20){
  params <- pnbd.EstimateParameters(cal.cbs, params);
  LL <- pnbd.cbs.LL(params, cal.cbs);
  p.matrix.row <- c(params, LL);
  p.matrix <- rbind(p.matrix, p.matrix.row);
}
colnames(p.matrix) <- c("r", "alpha", "s", "beta", "LL");
rownames(p.matrix) <- 1:21;

# When visualizing the params as calculated over and over again, do they converge?
p.matrix;

pnbd.PlotTransactionRateHeterogeneity(params)
pnbd.PlotDropoutRateHeterogeneity(params)

pnbd.Expectation(params, t=52); # This calcualtes how many transactions we can expect to see from a new customer

# These commands let us estimate how many transactions we expect from this customer, 
# and if we expect them to be alive at the end of the HOLDOUT (not calibration) period.
x <- cal.cbs["1516", "x"]
t.x <- cal.cbs["1516", "t.x"]
T.cal <- cal.cbs["1516", "T.cal"]
pnbd.ConditionalExpectedTransactions(params, T.star = 52,
                                     x, t.x, T.cal)
censor <- 10
pnbd.PAlive(params, x, t.x, T.cal)
pnbd.PlotFrequencyInCalibration(params, cal.cbs, censor)

# Calculate holdout period information (covered on page 9 of the walkthrough:
# https://cran.r-project.org/web/packages/BTYD/vignettes/BTYD-walkthrough.pdf
elog <- dc.SplitUpElogForRepeatTrans(elog)$repeat.trans.elog;
x.star <- rep(0, nrow(cal.cbs));
cal.cbs <- cbind(cal.cbs, x.star);
elog.custs <- elog$cust;
for (i in 1:nrow(cal.cbs)){
  current.cust <- rownames(cal.cbs)[i]
  tot.cust.trans <- length(which(elog.custs == current.cust))
  cal.trans <- cal.cbs[i, "x"]
  cal.cbs[i, "x.star"] <- tot.cust.trans - cal.trans
}

T.star <- 68 # length of the holdout period
x.star <- cal.cbs[,"x.star"]
comp <- pnbd.PlotFreqVsConditionalExpectedFrequency(params, T.star,
                                                    cal.cbs, x.star, censor)

rownames(comp) <- c("act", "exp", "bin")
tot.cbt <- dc.CreateFreqCBT(elog)

# ...Completed Freq CBT
d.track.data <- rep(0, 7 * 135)
origin <- as.Date("2013-04-25")
for (i in colnames(tot.cbt)){
  date.index <- difftime(as.Date(i), origin) + 1;
  d.track.data[date.index] <- sum(tot.cbt[,i]);
}
w.track.data <- rep(0, 135)
for (j in 1:135){
  w.track.data[j] <- sum(d.track.data[(j*7-6):(j*7)])
}
T.cal <- cal.cbs[,"T.cal"]
T.tot <- 135
n.periods.final <- 135
inc.tracking <- pnbd.PlotTrackingInc(params, T.cal,
                                     T.tot, w.track.data,
                                     n.periods.final)
cum.tracking.data <- cumsum(w.track.data)
cum.tracking <- pnbd.PlotTrackingCum(params, T.cal,
                                     T.tot, cum.tracking.data,
                                     n.periods.final)
