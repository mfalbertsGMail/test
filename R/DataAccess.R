#' @title DataAccess: Object to connect and interact with the Capital database
#' @examples 
#' da <- DataAccess(connection_param=connectionString, fs_id_param=1)
#' DataAccess.fi_get_instrument(da)
#' @param connection_param The SQL Server connection string
#' @param fs_id_param The scenario ID
#' @export
DataAccess <- function(connection_param ="", fs_id_param=NULL)
{
  # save the object variables
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

#' Get fincancial instruments for the context
#' 
#' Takes a DataAccess object and as_of_ and returns a datatable
#' with the instruments.  Properties that are schedules are coalesced 
#' to a slingle value based on the as_of_
#' @param object The Solvas dataAccess object 
#' @param as_of_date The as_of_date to use for schedule data types
#' @return Data frame containing the financial instrument data
#' @import RODBC
#' @export
DataAccess.fi_instrument_as_of_get <- function(object, as_of_date=NULL) {
  object$is_init  == 0
    fi_instrument_init_sp(object$connection,object$fs_id)
  object$is_init = 1
  return(fi_instrument_get_sp(object$connection,object$fs_id))
}

#' Get economic assumptions for the context
#' 
#' @param object The Solvas dataAccess object 
#' @param transformation_sequence_order - an integer value found on the 'Transformations' screen
#' @param criteria_sequence_order - an integer value found on the 'Transformation Entries for Sequence' screen
#' @export
DataAccess.fs_assumptions_get <- function(object, transformation_sequence_order, criteria_sequence_order) {
  return(fs_assumptions_get_sp(object$connection, object$fs_id, transformation_sequence_order, criteria_sequence_order))
}