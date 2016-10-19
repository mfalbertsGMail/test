#' @title DataAccessS4: Object to connect and interact with the Capital database
#' DataAccessS4(connection_param, fs_id)
#' @export
DataAccess_S4 <- setClass(
    "DataAccess_S4",
    slots = list(connection_param = "character", fs_id_param="numeric"),
    validity = function(object)
    {
      return(TRUE)
    }
)

setGeneric(name = "fi_get_instrument",
           def=function(object)
             {
             standardGeneric("fi_get_instrument")
           })

setMethod("fi_get_instrument", "DataAccess_S4", function(object) { 
  fi_instrument_init_nc(object@connection_param,object@fs_id_param)
  return(fi_instrument_get_nc(object@connection_param,object@fs_id_param))
  
  })

# da_s4 <- new("DataAccess_S4", connection_param=connectionString, fs_id_param=1)

