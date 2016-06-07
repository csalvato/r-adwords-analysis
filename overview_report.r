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

bing_keywords_elog <- create_bing_event_log(from=start_date, to=end_date)
adwords_keywords_elog <- create_adwords_event_log(from=start_date, to=end_date)

###################################### CREATE AdWords Plots and CSVs ##############################################
PowerSupplyUtilities::search_engine_marketing_report(adwords_keywords_elog, bing_keywords_elog)