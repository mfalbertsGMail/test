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
#' Internal Solvas|Capital Function 
#' Load Property Values
#' @param connection_string - The string representing the connection string...
#' @param fs_id - The scenario id
#' @return A data frame containing the financial instrument data
#' @import RODBC
#' @export
#' @keywords internal
SPInstrumentPropertyLoad <- function(connection_string, fs_id) {
  cn <- odbcDriverConnect(connection_string)
  if (is.null(fs_id) == FALSE) {
    sp <- paste("EXEC [app_r].[FI_Financial_Instrument_Property_load] @fs_id = ", fs_id)
    sqlQuery(cn, sp, errors=TRUE)
  }
  odbcClose(cn)
}

#' Internal Solvas|Capital Function 
#' Get financial instruments 
#' @param connection_string  The string representing the connection string...
#' @param fs_id The scenario ID
#' @param effective_date - The date to use for schedule data types
#' @param effective_period - The period to use for schedule data types (1=first period)
#' @return data frame containing the financial instrument data
#' @import RODBC
#' @export
#' @keywords internal
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

#' Internal Solvas|Capital Function 
#' gets Solvas|Capital assumptions 
#' @param connection_string  - the string representing the connection string...
#' @param fs_id The scenario ID
#' @param tm_desc - description/name of transformation (i.e. '(apply to all)').  the first
#  transformation by sequence order will be used if NULL, if two or more transformations have the
#  same description an error message is returned
#' @param use_dates - if true then matrix columns will be dates, else periods. default to false 
#' @return data frame containing the economic factors
#' @import RODBC
#' @import reshape2
#' @export
#' @keywords internal
SPFSAssumptionsGet <- function(connection_string, fs_id, tm_desc, use_dates) {
  cn <- odbcDriverConnect(connection_string)
  # make this a SP when done testing
  sp <- paste("EXEC [app_r].[FS_Assumptions_get] ",
                "@fs_id = ",  fs_id, ", ",
                "@tm_desc = ", ifelse(is.null(tm_desc), "NULL", paste("'",tm_desc,"'"))
              )
  data <- sqlQuery(cn, sp, errors=TRUE)
  # reshape so rather than rows of data it is by period in the column
  if (use_dates == TRUE) {
    data <- reshape2::dcast(data, property_code ~date, value.var = 'unified_value')
  } else {
    data <- reshape2::dcast(data, property_code ~period, value.var = 'unified_value')
  }
  rownames(data) <- data[,'property_code']
  odbcClose(cn)
  return(data)
}