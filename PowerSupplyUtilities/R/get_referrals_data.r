#' Get Referrals Data
#'
#' Returns a data frame with information about the number of referees (people who purchased from a referral) a user has accumulated within
#' within a date range.  Data is pulled from cached data warehouse that is updated nightly.
#'
#' @param from Start date in either format <"yyyy-mm-dd"> or <yyyymmdd>. Inclusive.
#' @param to End date in either format <"yyyy-mm-dd"> or <yyyymmdd>. Inclusive.
#' @param database_driver Path to the database driver for the database being used. Defaults to "database_drivers/postgresql-9.4.1208.jre6.jar".
#' @param jdbc_config_file Path to a text file with the URI to connect to the database, including credentials. Defaults to "jdbc_data_warehouse_database_config.txt"
#' @param transactions_query_file Path to a .sql file that includes the SQL query to be used to retrieve the necessary data from the database. Defaults to "referrals_query.sql"
#' @return A data frame with transaction data within the date range.
#' @export
#' @examples
#' get_transactions_data(from=20150101, to=20151231)

get_referrals_data <- function(from=Sys.Date(), 
                                  to=Sys.Date(),
                                  database_driver="database_drivers/postgresql-9.4.1208.jre6.jar",
                                  jdbc_config_file="jdbc_datawarehouse_string.txt",
                                  transactions_query_file="referrals_query.sql"){
  if(file.exists(database_driver)) {
    require(RJDBC)
    pgsql <- JDBC("org.postgresql.Driver", database_driver, "`")
    if(file.exists(jdbc_config_file)) {
      require(RPostgreSQL)
      db <- dbConnect(pgsql, string_from_file(jdbc_config_file))    
      if(file.exists(transactions_query_file)) {
        require(SalvatoUtilities)
        referrals_query <- string_from_file(transactions_query_file)
        referrals_data <- dbGetQuery(db, referrals_query)
        dbDisconnect(db)
        return(referrals_data)
      } else {
        dbDisconnect(db)
        stop("Can't find referrals_query.sql (or the file provided) in your working directory.\n\tDownload the file from the git repo (https://github.com/powersupplyhq/adwords-analysis), put it in your working directory and try again.")
      }
    } else {
      stop("Can't find the jdbc_data_warehouse_database_config.txt (or the file provided) in your working directory.\n\tDownload the file, or request it from Chris (chris@mypowersupply.com), put it in your working directory and try again.")
    }
  } else {
    if(database_drive == "database_drivers/postgresql-9.4.1208.jre6.jar") {
      stop("Can't find postgresql driver at location `database_drivers/postgresql-9.4.1208.jre6.jar` in your working directory.\n\tDownload the driver file, put it in that location and try again.")
    } else {
      stop("Can't find the postgresql driver at the location provided.")
    }
    
  }
}