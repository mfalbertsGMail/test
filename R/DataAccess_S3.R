#' @title DataAccess_S3: Object to connect and interact with the Capital database
#' @examples 
#' da <- Solvas.Capital.SQLUtility::DataAccess_S3(connectionString, fs_id)
#' da$fi_instrument_get()
#' @param connection_param The SQL Server connection string
#' @param fs_id_param The scenario ID
##'
##' @details text describing parameter inputs in more detail.
##' \itemize{
##'  \item{"parameter 1"}{Stuff}
##'  \item{"parameter 2"}{Stuff}
##'  \item da <- Solvas.Capital.SQLUtility::DataAccess_S3(connectionString, fs_id)
##'  \item da$fi_instrument_get()
##' }
##'
#' @export
#' @docType methods
#' @rdname DataAccess_S3-methods
DataAccess_S3 <- function(connection_param ="", fs_id_param=NULL)
{
  ## Get the environment for this instance of the function
  thisEnv <- environment()
  
  # save the object variables
  connection <- connection_param
  fs_id <- fs_id_param
  # try to create a connection
  cn <- odbcDriverConnect(connection)
  odbcClose(cn)
  # locals
  is_init <- 0
  
  # create the list used to represent the class
  me <- list( 
              thisEnv = thisEnv,
              
              getEnv = function()
              {
                return(get("thisEnv",thisEnv))
              },
              
              getConnection = function()
              {
                return(get("connection", thisEnv))
              },
              
              setConnection = function(value)
              {
                return(assign("connection", value, thisEnv))
              },
              
              getfs_id = function()
              {
                return(get("fs_id", thisEnv))
              },
              
              setfs_id = function(value)
              {
                return(assign("fs_id", value, thisEnv))
              },
              
              fi_instrument_get = function()
              {
                if (get("is_init", thisEnv) == 0)
                  fi_instrument_init_nc(connection,fs_id)
                assign("is_init", 1, thisEnv)
                return(fi_instrument_get_nc(connection,fs_id))
              }
              
              
            )
  ## define the value of the list within the current environment
  assign('this',me,envir=thisEnv)
  ## set the name for the class
  class(me) <- append(class(me), "DataAccess_S3")
  return(me)
}
#' Test S3 'method'
#' @param SC Data Access object
#' @export
DataAccess.Foo <- function(object) { return(object.connection) }