
#  !!!! REMOVE THIS FOR REAL HEADER
#rm(list = ls(pattern = "sc_*")) # remove all variables that begin with sc_
# declare for local testing of call from server
#sc_is_running_from_server = 1
#sc_connection_string = "Driver={Sql Server};server=(local);trusted_connection=True;database=Internal_Capital_DEV;"
#sc_event_id = NULL
#sc_fs_id = 1
#end 
###################################################################################################
# START OF SOLVAS|CAPITAL HEADER - DO NOT MODIFY 
#
# Variables that start with sc_ should be considered 'Solvas|Capital' system variables. Naming user
# variables with sc_ should be avoided.
#
# System variables passed from the server as parameters when running a scenario:
#
# sc_connection_string      - string = the connection string used connect back to the sever
# sc_is_running_from_server - bit - 1 = this script is being called from the server
#                                   0 = local development (or undefined)
# sc_fs_id                  - int - the id of the scenario being processed.
# sc_event_id               - int - used to report event messages to server 
# 
###################################################################################################
library(RODBC)
library(Solvas.Capital.SqlUtility)

if (exists('sc_is_running_from_server') && sc_is_running_from_server == 1)
{
  # declare diagnostic variables 
  sc_undefined = "undefined"
  sc_da <- DataAccess(connection_string_param = sc_connection_string, fs_id_param = sc_fs_id)
  sc_da_connection_status <- DataAccess.ConnectionStatus(sc_da)
  sc_libPath = .libPaths()
  
  # collect all the variables in the environment that have sc_ prefix
  sc_var_name = c(unlist(ls(pattern = "sc_*"), use.names = FALSE))
  # set vector with variable values, use sc_undefined if variable does not exist - get0 checks if the var name is a variable ifnotfound is a parameter name to get0
  sc_var_value = unname(sapply(sc_var_name,
                             function(x) 
                             {  
                               ifelse(is.null(get0(x, ifnotfound = sc_undefined)), 
                                      sc_undefined, 
                                      toString(get0(x, ifnotfound = sc_undefined)))
                             }))
  sc_result_set = data.frame(sc_var_name, sc_var_value, stringsAsFactors = FALSE)
  print(sc_result_set)
  # Push results to the SQL Server parameter
  sc_output_table <- as.data.frame(sc_result_set)
} else
  
  ###################################################################################################
  # END OF SOLVAS|CAPITAL HEADER - DO NOT MODIFY 
  ###################################################################################################
  
  ###################################################################################################
  #
  # Note: For local model development, modify the variables in this statement to point to the database
  # (sc_connection_string) and scenario ID (sc_fs_id)
  #
  ###################################################################################################
{
  sc_is_running_from_server = 0 
  sc_connection_string = "Driver={Sql Server};server=(local);trusted_connection=Yes;database=Internal_Capital_DEV;"
  sc_event_id = NULL
  sc_fs_id = 1
  sc_da <- DataAccess(connection_string_param = sc_connection_string, fs_id_param = sc_fs_id)
  sc_da_connection_status <- DataAccess.ConnectionStatus(sc_da)

}
 # Example...
if (sc_da_connection_status == "success") {
  # do model development here...

  # example: get and print out all assumptions
  assumptions = DataAccess.FsAssumptionsGet(sc_da, NULL, FALSE)
  print(assumptions)

  # example: get and print out interest_rate_effective value for all loans for period 1
  instruments = DataAccess.FiInstrumentGet(sc_da,NULL,1)
  print(instruments['interest_rate_effective'])
  
  # example: get and print out interest_rate_effective value for all loans for effective date 9/30/2014
  instruments = DataAccess.FiInstrumentGet(sc_da,'9/30/2014',NULL)
  print(instruments['interest_rate_effective'])

} else {
  print(paste("There was an error connecting:", sc_da_connection_status))
}
