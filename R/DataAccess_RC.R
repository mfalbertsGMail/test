#' A DataAccess Reference class example
#' @name DataAccess_RC
#' @examples 
#' da_rc <- new ("DataAccess_RC", connection_param=connection_string, fs_id_param=1)
#' da_rc$fi_instrument_get('1/1/2014')
#' @import methods
#' @exportClass DataAccess_RC
#' @field connection_param connection string...
#' @field fs_id_param  fs id....
#' @import RODBC
#' @export
# Test with 'reference class' objects
# Pros: more like oop
# Cons: can't use roxygen2 do document methods in-line - could do it in the header, didn't see a way to declare 
#       local variables

DataAccess_RC <- setRefClass("DataAccess_RC", 
                             fields = list(connection_param = "character", fs_id_param="numeric"),
                             methods = list(
                               fi_instrument_initialize = function()
                               {
                                 "initializes the database "
                                 return(fi_instruments_init(connection_param,fs_id));
                               },
                               fi_instrument_get = function(as_of_date)
                               {
                                 "returns a datatable with the instruments for the passed as_of_date"
                                 return(fi_instrument_get_nc(connection_param,fs_id));
                               }
                             )
)