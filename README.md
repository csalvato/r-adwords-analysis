## AdWords Analysis

This project contains code related to running PPC analysis.  As of this writing, that includes only AdWords.

The analysis is broken down into 4 distinct steps:

1. Get the data
2. Format and enrich the data (including the creation of an `event log` aka `elog`)
3. Create data frames for tabulation/visualization
4. Create visualizations - tables (.csv files) and graphical plots

# Event Logs
## Event Log Basics

An event log can be thought of as a chronological ledger. Whenever an event happens, it is marked in the ledger.  A good example of this is a transaction log whenever someone orders from Power Supply:

| Date                    | Transaction Amount | Recurring? | Meals                             | User ID | User Name | Campaign ID | Campaign Name       | 
|-------------------------|--------------------|------------|-----------------------------------|---------|-----------|-------------|---------------------|
| 2016-01-01 06:04:15 UTC | $50.00             | Yes        | Paleo - 5 Lunches, 5 Dinners      | 1       | Alice     | 1           | DC Paleo Performers |
| 2016-02-01 10:29:12 UTC | $50.00             | Yes        | Paleo - 5 Lunches, 5 Dinners      | 1       | Alice     | 1           | DC Paleo Performers |
| 2016-02-05 00:10:40 UTC | $200.00            | No         | Mixitarian - 3 Lunches, 3 Dinners | 2       | Bob       | 4           | LA Paleo Performers |


In the above transaction log, every time a single person takes an action, it's logged as an event with the exact *second* the event occurred.  Pretty basic stuff.

But it doesn't always need to be that a single event is logged at a single point in time down to the second.  It can be that all events that happen in a day are logged togther.  For example, AdWords reports their events in aggregate, and you can specify the granularity (e.g. daily):

| Date       | Impressions | Clicks | Cost    | Campaign ID | Campaign Name       |
|------------|-------------|--------|---------|-------------|---------------------|
| 2016-01-01 | 1000        | 10     | $10.00  | 1           | DC Paleo Performers |
| 2016-02-01 | 5000        | 80     | $88.00  | 1           | DC Paleo Performers |
| 2016-02-05 | 5200        | 60     | $54.00  | 4           | LA Paleo Performers |

If we want to look at these events together, we can combine them into a single event log.  Just two simple rules for doing so:

1. Formatting for common columns must match (e.g. Campaign ID, Campaign Name and Date) so that we can group the data together
2. Variables that don't apply to certain observations must be marked as NA.

Here's an example of how the two sample Event Logs can be combined together, once we make sure the date columns are in a similar format of YYYY-MM-DD.

| Date       | Transaction Amount | Recurring? | Meals                             | User ID | User Name | Campaign ID | Campaign Name       | Impressions | Clicks | Cost    |
|------------|--------------------|------------|-----------------------------------|---------|-----------|-------------|---------------------|-------------|--------|---------|
| 2016-01-01 | $50.00             | Yes        | Paleo - 5 Lunches, 5 Dinners      | 1       | Alice     | 1           | DC Paleo Performers | NA          | NA     | NA      |
| 2016-02-01 | $50.00             | Yes        | Paleo - 5 Lunches, 5 Dinners      | 1       | Alice     | 1           | DC Paleo Performers | NA          | NA     | NA      |
| 2016-02-05 | $200.00            | No         | Mixitarian - 3 Lunches, 3 Dinners | 2       | Bob       | 4           | LA Paleo Performers | NA          | NA     | NA      |
| 2016-01-01 | NA                 | NA         | NA                                | NA      | NA        | 1           | DC Paleo Performers | 1000        | 10     | $10.00  |
| 2016-02-01 | NA                 | NA         | NA                                | NA      | NA        | 1           | DC Paleo Performers | 5000        | 80     | $88.00  |
| 2016-02-05 | NA                 | NA         | NA                                | NA      | NA        | 4           | LA Paleo Performers | 5200        | 60     | $54.00  |

This enables us to group columns together to quickly visualize the data.  For example, let's ignore the NA values and group by Campaign Name and sum the columns for Impressions, Cost, Clicks and Transaction Amount:

```R
library(dplyr)

event_log %>% 
    group_by(campaign_name) %>%
    summarize(impressions=sum(impressions,na.rm=TRUE),
              cost=sum(cost,na.rm=TRUE),
              clicks=sum(clicks,na.rm=TRUE),
              amount=sum(transaction_amount,na.rm=TRUE),
              )
```

The result would be:

| Campaign Name       | impressions | cost    | clicks | amount |
|---------------------|-------------|---------|--------|--------|
| DC Paleo Performers | 6000        | $100.00 | 90     | 100    |
| LA Paleo Performers | 5200        | $200.00 | 60     | 200    |


# AdWords Event Logs

In our AdWords analysis, we create an Event Log that ties together keyword events from AdWords and transaction events from the database powering Power Supply's ordering application.  Since AdWords doesn't know anything about the application's users, and the application knows nothing about AdWords campaigns, we need something to tie users back to the campaign where they originated.

To tie these two data sources together, we use Mixpanel to capture information about the user when they click on an ad.  We use that user information to tie the user's transaction behavior to the ad's cost.

In other words, we pull from 3 different data sources (AdWords, Mixpanel and Power Supply's orders app) to create a unified event log.  The resulting event log can be created in a single line of code:

`keywords_elog <- create_event_log(from=start_date, to=end_date)`

The resulting data frame includes the following variables that can be grouped and/or aggregated to create tables and visualizations:

1. campaign_id - The ID of the campaign. Not intended to be human readable. Used to join campaign data to other data sets based on ID.
1. campaign_name - The name of the campaign.
1. user_name - The user's name
1. user_id - The user's ID in the orders app
1. discount_amount - If this observation is a purchase, this is the amount of any discount applied.
1. credit_used - If this observation is a purchase, this is the amount of any credit applied.
1. sales_tax_amount - If this observation is a purchase, this is the amount of any sales tax applied.
1. refund_amount - If this observation is a refund, this is the amount of the refund.
1. retail_price - If this observation is a purchase, this is the retail price (not including any discounts or credits) of the item.
1. money_in_the_bank_paid_to_us - If this observation is a purchase, this is the amount of money paid to use after discounts and credits.
1. purchase_type -  - If this observation is a purchase, was it a recurring purchase or a one-off purchase? NOTE: recurring does not necessarily mean they paid more than once, but that they selected the "recurring" option to get 5% off their order.
1. X.browser - During the user's first order, this is the browser the user was using.
1. X.browser_version - During the user's first order, this is the version of the browser the user was using.
1. X.city - During the user's first order, this is the city the user was in, based on the users's IP.
1. X.current_url - During the user's first order, this is the URL a users's browser was on when completing the event.
1. X.initial_referrer - During the user's first order, this is the URL that the user was initially referred from.
1. X.initial_referring_domain - During the user's first order, this is the domain that the user was initially referred from.
1. X.os - During the user's first order, this is the OS that the user was using.
1. X.referrer - During the user's first order, this is the URL of the site the user was most recently referred from
1. X.referring_domain - During the user's first order, this is the domain of the site the user was most recently referred from
1. X.region - During the user's first order, this is the region the user was in at the time of the first purchase.
1. X.screen_height - During the user's first order, this is the screen height of the screen the user was using.
1. X.screen_width - During the user's first order, this is the screen width of the screen the user was using.
1. X.search_engine - During the user's first order, this is the search engine the user was using to land on the site
1. latest_ad_awcreativeid - This is the ID of the latest ad (aka Creative) the user had seen before placing their first order.
1. keyword - This is the name of the most recent keyword that the user had matched with their search term, and clicked on before placing their first order.
1. match_type - This is the match type (`Broad`, `Exact` or `Phrase`) of the most recent keyword that the user had matched with their search term, and clicked on before placing their first order.
1. latest_ad_awnetwork - This is the network on which the user had seen an before placing their first order. Values are `g` (Google), `s` (Search Partners) and `d` (Display Network)
1. latest_ad_awposition - The position of the ad the user had clicked on before placing their first order, with a value like "1t2"(this means page 1, top, position 2)
1. device - The device the user was using to place their first order.
1. latest_ad_psgeo - The PSGeo (power supply geo) that the user was located in (according to campaign targetting) when placing their first order.
1. latest_ad_search - The full search string (i.e. everything after the `?` in a tracking URL) associated with the most recent ad the user had experienced.
1. latest_ad_utm_medium - The medium on which the user had seen an ad before making their first purchase. (e.g. cpc = cost per click advertising)
1. latest_ad_utm_source - The source from which the user had seen an ad before making their first purchase. (e.g. Google)
1. orderId - The ID of the order in our orders application

** More Data Fields to be defined as they are needed.  For explanation, reach out to Chris Salvato (chris@mypowersupply.com) and then add the definition to the list below in README.md**

1. utm_campaign
1. utm_medium
1. utm_source
1. X.device
1. ad_group_id
1. date
1. day_of_week
1. keyword_state
1. ad_group_name
1. cost
1. network
1. est_search_impression_share
1. est_search_impression_share_lost_rank
1. impressions
1. clicks
1. average_position
1. quality_score
1. landing_page_experience
1. week
1. referred_users_ordering
1. new_referred_users
1. total_referred_users
1. new_referree_sales
1. new_referree_orders
1. referred_users_meals
1. referred_users_orders
1. referred_users_discounted_price
1. referred_users_price_amount
1. referred_users_transaction_amount
1. influencer_meals
1. influencer_orders
1. influencer_discounted_price
1. influencer_price_amount
1. influencer_transaction_amount


Here's a short glossary of data that we pull from various sources.

## AdWords
	
	To be written...

## Mixpanel

	To be written...

## Transactions (from Heroku Database)
	
	To be written...

# Configuration Files

Several functions within this project pull data from external APIs such as Mixpanel and AdWords.  To manage the credentials for these APIs, we keep all credentials in YAML files to read the data. Since these YAML files contain sensitive data, they are not included in our git repository.

To run the code within this project, you will need the following files. They can be obtained by reaching out to Chris (@chris on Slack; chris@mypowersupply.com via email):

1. adwords_credentials.yml
2. .google.auth.RData (for instant google Authentication)
2. mixpanel_credentials.yml
3. jdbc_heroku_string.txt
4. jdbc_datawarehouse_string.txt