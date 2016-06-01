#' Get First Transactions Dates for Users
#'
#' Using a vector of user IDs, this function returns a data frame with two columns: 
#' the user's ID and the date of their first transaction if their first transaction
#' falls within the from and to dates.  If their first transaction falls outside of those dates.
#' the user's information is not included.
#'
#' @param from Start date in either format <"yyyy-mm-dd"> or <yyyymmdd>. Inclusive.
#' @param to End date in either format <"yyyy-mm-dd"> or <yyyymmdd>. Inclusive.
#' @param users A vector of user IDs that should be in the database.
#' @param database_driver Path to the database driver for the database being used. Defaults to "database_drivers/postgresql-9.4.1208.jre6.jar".
#' @param jdbc_config_file Path to a text file with the URI to connect to the database, including credentials. Defaults to "jdbc_data_warehouse_database_config.txt"
#' @return A data frame with transaction data within the date range.
#' @export
#' @examples
#' get_first_transaction_dates_for_users(from=20150101, to=20151231, users=transactions$users)

get_first_transaction_dates_for_users <- function(from=Sys.Date(), 
                                  to=Sys.Date(),
                                  users = NULL,
                                  database_driver="database_drivers/postgresql-9.4.1208.jre6.jar",
                                  jdbc_config_file="jdbc_heroku_string.txt"){

  if(file.exists(database_driver)) {
    require(RJDBC)
    pgsql <- JDBC("org.postgresql.Driver", database_driver, "`")
    if(file.exists(jdbc_config_file)) {
      require(RPostgreSQL)
      db <- dbConnect(pgsql, string_from_file(jdbc_config_file))    
      if(!is.null(users)) {
        require(SalvatoUtilities)
        data <- dbGetQuery(db, GetoptLong::qq(paste("SELECT 
                                                        * 
                                                     FROM
                                                       (select 
                                                       min(t.created_at) as first_transaction,
                                                       u.id,
                                                       u.name
                                                       from users u
                                                       inner join
                                                       transactions t on t.user_id = u.id
                                                       where
                                                       u.id IN (", paste(shQuote(users, type = "sh"), collapse=','),
                                                                ")
                                                       group by u.id
                                                       ORDER BY first_transaction desc) first_transactions
                                                     WHERE
                                                      first_transaction between '@{start_date}' and '@{end_date}'")))
        dbDisconnect(db)
        return(data)
      } else {
        dbDisconnect(db)
        stop("No vector of user IDs has been provided. Unable to proceed.")
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