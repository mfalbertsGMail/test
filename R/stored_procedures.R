# ' Low-level access to the database stored procedures.  Not public - use 
# ' the object model to access these methods.
# ' 
# ' Initalizes the financial instrument records - must be done on scenarios
# ' that have not been run
# ' 
# ' Takes a connection string (connectionString) and scenario id (fs_id) and 
# ' initializes the financial instruments on the server.  This *should* not 
# ' be executed on a scenario that has already been run as it will wipe out the
# ' snapshot. 
# '  
# ' @param connectionString  The string representing the connection string...
# ' @param fs_id The scenario ID
# ' @return Data frame containing the financial instrument data
#' @import RODBC

fi_instrument_init_sp <- function(connectionString, fs_id) {
  cn <- odbcDriverConnect(connectionString)
  if (is.null(fs_id) == FALSE) {
    # may want to put this into a seperate 'init' routine.
    sp <- paste("EXEC [app_r].[FI_Financial_Instrument_Property_load] @fs_id = ", fs_id)
    sqlQuery(cn, sp, errors=TRUE)
  }
  odbcClose(cn)
}

# ' Get fincancial instruments for a specific screnario ID and 
# ' 
# ' Takes a connection string (connectionString) and scenario id (fs_id) and returns a datatable
# ' with the instruments
# ' @param connectionString  The string representing the connection string...
# ' @param fs_id The scenario ID
# ' @return Data frame containing the financial instrument data
# ' @import RODBC

fi_instrument_get_sp <- function(connectionString, fs_id) {
  cn <- odbcDriverConnect(connectionString)
  # note: this will not return anything if the scenario has not been run or fi_instruments_init_sp has not been called
  #TODO - use FI_Financial_instrument but put extra parameter @use_effective_scalar_value)
  sp <- paste("EXEC [app_r].[FI_Financial_Instrument_as_of_get] @fs_id = ", ifelse(is.null(fs_id), "NULL", fs_id))
  data <- sqlQuery(cn, sp, errors=TRUE)
  odbcClose(cn)
  return(data)
}
   
# ' Gets Capital assumptions schedules for the specified connection string, scenario and transformation (by sequence order)
# ' TODO: use a SP and make this work on Description rather than transformation sequence! 
# ' Takes a connection string (connectionString) and scenario id (fs_id) and returns a datatable
# ' with the instruments
# ' @param connectionString  - the string representing the connection string...
# ' @param fs_id The scenario ID
# ' @param transformation_sequence_order Way to identify the the transformation entry to get the criteria from 
# ' @param criteria_sequence_order Way to identify the the criteria entry to get the characteristics from 
# ' @return Data frame containing the economic factors
# ' @import RODBC
# ' @import reshape2
# ' 
fs_assumptions_get_sp <- function(connectionString, fs_id, transformation_sequence_order, criteria_squence_order) {
  cn <- odbcDriverConnect(connectionString)
  # make this a SP when done testing
  sp <- paste("SELECT property_code, period, unified_value FROM app.tf_capital_schedule_history(", fs_id, ",", transformation_sequence_order, ",", criteria_squence_order,  ") WHERE DATE IS NULL ORDER BY property_code, period")
  data <- sqlQuery(cn, sp, errors=TRUE)
  # reshape so rather than rows of data it is by period in the column
  data <- reshape2::dcast(data, property_code ~period, value.var = 'unified_value')
  rownames(data) <-data[,'property_code']
  odbcClose(cn)
  return(data)
}