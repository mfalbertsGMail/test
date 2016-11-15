#' The DataAccess object is the main object that must be created in order to use any of 
#' the other methods.  This object is always the first parameter passed to any of the 
#' the methods.
#' @title DataAccess: da_obj to connect and interact with the Capital database
#' @examples
#' connectionString = "Driver={Sql Server};server=(local);trusted_connection=Yes;database=Internal_Capital_DEV;"
#' sc_da <- DataAccess(connection_param=connectionString, fs_id_param=1)
#' DataAccess.fi_instrument_get(sc_da, NULL, 1)
#' @param connection_param The SQL Server connection string
#' @param fs_id_param The scenario ID
#' @export
DataAccess <- function(connection_param ="", fs_id_param=NULL)
{
  # save the da_obj variables
  me <- list(
    connection = connection_param,
    fs_id = fs_id_param,
    # locals
    is_init = 0
  )

  ## set the name for the class
  class(me) <- append(class(me), "DataAccess")
  return(me)
}


#' Returns the connection status for current da_obj connection string
#' 
#' Takes a DataAccess da_obj and returns "success" if the connection is 
#' working otherwise the error message is returned.
#' @param da_obj - The Solvas dataAccess object 
#' @return connection status string "success" if connection is valid otherwise an error message is returned
#' @import RODBC
#' @export
DataAccess.connection_status <- function(da_obj) {
  return 
    tryCatch(
      {
        cn <- odbcDriverConnect(da_obj$connection)
        odbcClose(cn)
        "success"
      } ,  
      warning = function(cond) paste("ERROR:  ", cond),
      error = function(cond)  paste("ERROR:  ", cond)	  
    )
}

#' Get fincancial instruments for the context
#' 
#' Takes a DataAccess da_obj and effective_date or effective_period  and returns a datatable
#' with the instruments.  Properties that are schedules are coalesced 
#' to a single value based on the effective_date or effective_period. 
#' NOTE: EITHER effective_date or effective_period must be poplulate the other one must be NULL
#' @param da_obj - The Solvas dataAccess object 
#' @param effective_date = Used for to resolve schedules column values to a single scalar value
#' @param effective_period = Used for to resolve schedules column values to a single scalar value (1=first period)
#' @return Data frame containing the financial instrument data
#' @import RODBC
#' @export
DataAccess.fi_instrument_get <- function(da_obj, effective_date = NULL, effective_period = NULL) {
  da_obj$is_init  == 0
    sp_fi_instrument_init(da_obj$connection,da_obj$fs_id)
  da_obj$is_init = 1
  return(sp_fi_instrument_get(da_obj$connection,da_obj$fs_id, effective_date, effective_period))
}

#' Get economic assumptions for the context
#' 
#' @param da_obj - The Solvas dataAccess object 
#' @param transformation_sequence_order - an integer value found on the 'Transformations' screen
#' @param criteria_sequence_order - an integer value found on the 'Transformation Entries for Sequence' screen
#' @export
DataAccess.fs_assumptions_get <- function(da_obj, transformation_sequence_order, criteria_sequence_order) {
  return(sp_fs_assumptions_get(da_obj$connection, da_obj$fs_id, transformation_sequence_order, criteria_sequence_order))
}