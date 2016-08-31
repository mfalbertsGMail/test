#' Create the connection object used for all the database functions
#' 
#' Takes a connection string and returns a connection object
#' @param connectionString  - the string representing the connection string...
#' @return The connection object
#' @import RODBC
#' @export

connection <- function(connectionString){
  return(odbcDriverConnect(connectionString))
}

#' Get fincancial instruments for a specific screnario ID and 
#' 
#' Takes a connection string (connectionString) and scenario id (fs_id) and returns a datatable
#' with the instruments
#' @param connectionString  - the string representing the connection string...
#' @param fs_id The scenario ID
#' @return Data frame containing the financial instrument data
#' @import RODBC
#' @export

fi_instrument_get <- function(connectionString, fs_id) {
  conn <- odbcDriverConnect(connectionString)
  cn <- odbcDriverConnect(connectionString)
  sp <- paste("EXEC [app].[FI_Financial_Instrument_get] @fs_id = ", ifelse(is.null(fs_id), "NULL", fs_id))
  data <- sqlQuery(cn, sp, errors=TRUE)
  odbcCloseAll()
  return(data)
}
  
#' Get economic factor schedules for the specified connection string, scenario and transformation (by sequence order)
#' 
#' Takes a connection string (connectionString) and scenario id (fs_id) and returns a datatable
#' with the instruments
#' @param connectionString  - the string representing the connection string...
#' @param fs_id The scenario ID
#' @param transformation_sequence_order Way to identify the the transformation entry to get the criteria from 
#' @param criteria_sequence_order Way to identify the the criteria entry to get the characteristics from 
#' @return Data frame containing the financial instrument data.
#' @import RODBC
#' @import reshape2
#' @export
#' 
fs_economic_factors_get <- function(connectionString, fs_id, transformation_sequence_order, criteria_squence_order) {
  conn <- odbcDriverConnect(connectionString)
  cn <- odbcDriverConnect(connectionString)
  # make this a SP when done testing
  sp <- paste("SELECT property_code, period, rate FROM app.tf_capital_schedule_history(", fs_id, ",", transformation_sequence_order, ",", criteria_squence_order,  ") WHERE DATE IS NULL ORDER BY property_code, period")
  data <- sqlQuery(cn, sp, errors=TRUE)
  # reshape so rather than rows of data it is by period in the column
  data <- reshape2::dcast(data, property_code ~period, value.var = 'rate')
  odbcCloseAll()
  return(data)
}