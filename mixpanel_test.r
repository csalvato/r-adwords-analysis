require(RMixpanel)

account = mixpanelCreateAccount("Power Supply - Main",
                                token="3fdbf9929d332c37f82380157a564049",
                                secret="af3b32cc21c7b6e91b71f7c0417735d2", 
                                key="ce370ab09a166e168d448080b55715f6")

events_aw <- mixpanelGetEvents(account, 
                  from=20151217,
                  to=20160503,
                  event=array("Completed Order"),
                  where='(properties["latest_ad_search"]) and (properties["latest_ad_utm_source"] == "Google")')

#TODO: Should be filtered by start_date and end_date
events_aw_df <- data.frame(events_aw)
events_aw_df  <- events_aw_df %>% rename(app_user_id=id)

pgsql <- JDBC("org.postgresql.Driver", "../database_drivers/postgresql-9.4.1208.jre6.jar", "`")
datawarehouse_db <- dbConnect(pgsql, string_from_file("jdbc_datawarehouse_string.txt"))

mixpanel_test_dw_data <- dbGetQuery(datawarehouse_db, string_from_file("mixpanel_test_query.sql"))

dbDisconnect(datawarehouse_db)

# This is convering app user id into much smaller numbers...What is going on?  Maybe converting it to factor number?  
# I think as.numeric is wrong..
events_aw_df  <- events_aw_df %>% mutate(app_user_id = as.numeric(as.character(app_user_id)))
unique_users <- distinct(events_aw_df, app_user_id)

mixpanel_test_data  <- mixpanel_test_dw_data %>% inner_join(unique_users, by="app_user_id")


# events_bing <- mixpanelGetEvents(account, 
#                                from=20151217,
#                                to=20160503,
#                                event=array("Completed Order"),
#                                where='(properties["latest_ad_search"]) and (properties["latest_ad_utm_source"] == "Bing")')
# 
# events_bing_df <- data.frame(events_bing)
