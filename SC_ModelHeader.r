
# ## # setup sc_result_set for returning values
#!!!! REMOVE THIS FOR REAL HEADER
rm(list = ls(pattern = "sc_*")) # remove any sc_ from the env

#sc_is_running_from_server = 1; # for local testing! remove before submitting model

if (exists('sc_is_running_from_server') == FALSE || sc_is_running_from_server == 0)  
{
  #sc_output_table = NULL
  sc_is_running_from_server = 1 # test setting to 1
  #sc_connection_string = "Driver=SQL Server Native Client 11.0;server=(local);database=Internal_Capital_Dev;UId=solvas_user;Pwd=solvas"
  
  #' setup local connection data for model development and testing
  sc_connection_string = "Driver={Sql Server};server=(local);trusted_connection=Yes;database=Internal_Capital_DEV;"
  sc_event_id = 25
  sc_fs_id = 12
}
#  # end setup var for testing

###################################################################################################
# START OF SOLVAS|CAPITAL HEADER - DO NOT MODIFY 
#
# Variables that start with sc_ should be considered 'Solvas|Capital' system variables. Naming your
# variables with sc_ should be avoided.
#
# System Variables passed from the Server as parameters when running a scenario:
#
# sc_is_running_from_server - bit - 1 = this script is being called from the server, 0 or undefined 
#   = local develpment)
# sc_fs_id - int - the ID of the scenario being processed.
# sc_event_id - int - used to report back log messages to server 
# 
###################################################################################################

if (exists('sc_is_running_from_server') && sc_is_running_from_server == 1)
{
  # declare diagnostic variables 
  sc_undefined = "undefined"
  
  # check for required library RODBC
  sc_diag_rodbc_lib =
    tryCatch(
      {
        library(RODBC)
        "success"
      },  
      warning = function(cond) paste("ERROR:  ", cond),
      error = function(cond)  paste("ERROR:  ", cond) 
    )
  
  # check for required library Solvas.Capital.Utility
  sc_diag_solvas_utility_lib =
    tryCatch(
      {
        library(Solvas.Capital.SqlUtility)
        "success"
      },  
      warning = function(cond) paste("ERROR:  ", cond),
      error = function(cond) paste("ERROR:  ", cond)	  
    )
  # hard code this until the package is done...
 sc_diag_solvas_utility_lib = "success"
    
  
  # check for odbc connectivity
  sc_diag_connection = 
    tryCatch(
      {
        cn <- odbcDriverConnect(connection=sc_connection_string)
        print(sqlQuery(cn, "SELECT CURRENT_USER", errors=TRUE))
        odbcClose(cn)
        "success"
      } ,  
      warning = function(cond) paste("ERROR:  ", cond),
      error = function(cond)  paste("ERROR:  ", cond)	  
    )
  
  sc_current_db_user = 
    tryCatch(
      {
        cn <- odbcDriverConnect(connection=sc_connection_string)
        curr_user = sqlQuery(cn, "SELECT CURRENT_USER", errors=TRUE)
        odbcClose(cn)
        curr_user[[1]]
      } ,  
      warning = function(cond) paste("ERROR:  ", cond),
      error = function(cond)  paste("ERROR:  ", cond)	  
    )
  
# collect all the variables in the environment that have sc_ prefix
sc_var_name = c(unlist(ls(pattern = "sc_*"),use.names = FALSE))
# set vector with variable values, use sc_undefined if variable does not exist - get0 checks if the var name is a variable ifnotfound is a parameter name to get0
sc_var_value = unname(sapply(sc_var_name,
                             function(x) 
                             {  
                               ifelse(is.null(get0(x,ifnotfound=sc_undefined)), 
                                      sc_undefined, 
                                      toString(get0(x,ifnotfound=sc_undefined)))
                             }))
sc_result_set = data.frame(sc_var_name, sc_var_value, stringsAsFactors = FALSE)

print(sc_result_set)
# push the results to the SQL server parameter from sp_execute_external_script
sc_output_table <- as.data.frame(sc_result_set)
}
###################################################################################################
# END OF SOLVAS|CAPITAL HEADER - DO NOT MODIFY 
###################################################################################################
 #example...
  sc_da <- DataAccess(connection_param=sc_connection_string, fs_id_param=sc_fs_id)
  instruments = DataAccess.fi_instrument_get(sc_da,'1/1/2015')
  assumptions = DataAccess.fs_assumptions_get(sc_da,1,1)
  # print(instruments)
 # print(assumptions)
 # print(assumptions['LOSS_SEVERITY_RATE_BY_RELATIVE', '0'])
 
 ## cn <- odbcDriverConnect(sc_da$connection)
 ## # make this a SP when done testing
 ## sp <- paste("SELECT property_code, period, unified_value FROM app.tf_capital_schedule_history(", sc_da$fs_id, ",", 1, ",", 1,  ") WHERE DATE IS NULL ORDER BY property_code, period")
 ## data <- sqlQuery(cn, sp, errors=TRUE)
 ## print(data)
 ## # reshape so rather than rows of data it is by period in the column
 ## data <- reshape2::dcast(data, property_code ~period, value.var = 'unified_value')
 ## print(data)
 ## odbcClose(cn)
 ## 