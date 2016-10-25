#' @title DataAccess: Object to connect and interact with the Capital database
#' @examples 
#' da <- DataAccess(connection_param=connectionString, fs_id_param=1)
#' DataAccess.fi_get_instrument(da)
#' @param connection_param The SQL Server connection string
#' @param fs_id_param The scenario ID
#' @export
#' @docType methods
#' @rdname DataAccess-methods
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
#' Takes a connection string (connectionString) and scenario id (fs_id) and returns a datatable
#' with the instruments
#' @param object The Solvas dataAccess object 
#' @param as_of_date The as_of_date to use for schedule data types
#' @return Data frame containing the financial instrument data
#' @import RODBC
#' @export
DataAccess.fi_get_instrument <- function(object, as_of_date=NULL) {
  object$is_init  == 0
    fi_instrument_init_nc(object$connection,object$fs_id)
  object$is_init = 1
  return(fi_instrument_get_nc(object$connection,object$fs_id))
}