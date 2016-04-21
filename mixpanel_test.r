require(RMixpanel)

account = mixpanelCreateAccount("Power Supply - Main",
                                token="3fdbf9929d332c37f82380157a564049",
                                secret="af3b32cc21c7b6e91b71f7c0417735d2", 
                                key="ce370ab09a166e168d448080b55715f6")

events <- mixpanelGetEvents(account, 
                  from=20151217,
                  to=20160421,
                  event=array("Completed Order"),
                  where='(properties["latest_ad_search"]) and (properties["latest_ad_utm_source"] == "Google")')
