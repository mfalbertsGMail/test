#  Low-level access to the database stored procedures.  Not public - use 
#  the object model to access these functions. All functions that directly
#  call any SPs should be at this level. 
#  
#  Initializes the financial instrument records - must be done on scenarios
#  that have not been run
#  
#  Takes a connection string (connection_string) and scenario id (fs_id) and 
#  loads the financial instruments on the server. No action will take place
#  on a scenario that has already been run.
#   
#  @param connection_string - The string representing the connection string...
#  @param fs_id - The scenario id
#  @return A data frame containing the financial instrument data
#' @import RODBC
SPInstrumentPropertyLoad <- function(connection_string, fs_id) {
  cn <- odbcDriverConnect(connection_string)
  if (is.null(fs_id) == FALSE) {
    sp <- paste("EXEC [app_r].[FI_Financial_Instrument_Property_load] @fs_id = ", fs_id)
    sqlQuery(cn, sp, errors=TRUE)
  }
  odbcClose(cn)
}

#  Get financial instruments for a specific scenario ID and 
#  
#  Takes a connection string (connection_string) and scenario id (fs_id) and returns a data frame
#  with the instruments
#  @param connection_string  The string representing the connection string...
#  @param fs_id The scenario ID
#  @return data frame containing the financial instrument data
#' @import RODBC
SPFIInstrumentGet <- function(connection_string, fs_id,  effective_date, effective_period) {
  cn <- odbcDriverConnect(connection_string)
  # note: this will return zero rows if the scenario has not been run or fi_instruments_init_sp has not been called
  sp <- paste("EXEC [app_r].[FI_Financial_Instrument_Effective_Scalar_get] @fs_id = ", 
                fs_id, 
                ",@effective_scalar_date = ",ifelse(is.null(effective_date), "NULL", paste("'",effective_date,"'")), 
                ",@effective_scalar_period = ", ifelse(is.null(effective_period), "NULL", effective_period),
                ",@match_LTE = 1")
  data <- sqlQuery(cn, sp, errors=TRUE)
  odbcClose(cn)
  return(data)
}

#  Gets Capital assumptions schedules for the specified connection string, scenario and transformation (by sequence order)
#  TODO: use a SP and make this work on Description rather than transformation sequence! 
#  Takes a connection string (connection_string) and scenario id (fs_id) and returns a data frame
#  with the instruments
#  @param connection_string  - the string representing the connection string...
#  @param fs_id The scenario ID
#  @param transformation_sequence_order Way to identify the the transformation entry to get the criteria from 
#  @param criteria_sequence_order Way to identify the the criteria entry to get the characteristics from 
#  @return data frame containing the economic factors
#' @import RODBC
#' @import reshape2
SPFSAssumptionsGet <- function(connection_string, fs_id, transformation_sequence_order, criteria_squence_order) {
  cn <- odbcDriverConnect(connection_string)
  # make this a SP when done testing
  sp <- paste("SELECT property_code, period, unified_value FROM app.tf_capital_schedule_history(", fs_id, ",", transformation_sequence_order, ",", criteria_squence_order,  ") WHERE DATE IS NULL ORDER BY property_code, period")
  data <- sqlQuery(cn, sp, errors=TRUE)
  # reshape so rather than rows of data it is by period in the column
  data <- reshape2::dcast(data, property_code ~period, value.var = 'unified_value')
  rownames(data) <- data[,'property_code']
  odbcClose(cn)
  return(data)
}