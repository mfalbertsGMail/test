
# ## # setup sc_result_set for returning values
# rm(list = ls(pattern = "sc_*")) # remove any sc_ from the env

if (exists('sc_is_running_from_server') == FALSE || sc_is_running_from_server == 0)  
{
  #sc_output_table = NULL
  sc_is_running_from_server = 1 # test setting to 1
  #sc_connection_string = "Driver=SQL Server Native Client 11.0;server=(local);database=Internal_Capital_Dev;UId=solvas_user;Pwd=solvas"
  sc_connection_string = "Driver={Sql Server};server=(local);trusted_connection=Yes;database=Internal_Capital_DEV;"
  sc_event_id = 25
  sc_fs_id = 3
}
#  # end setup var for testing

###################################################################################################
# START OF SOLVAS|CAPITAL HEADER - DO NOT MODIFY 
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
        library(Solvas.Capital.SQLUtility)
        "success"
      },  
      warning = function(cond) paste("ERROR:  ", cond),
      error = function(cond) paste("ERROR:  ", cond)	  
    )
  
  
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
  
  sc_diag_current_db_user = 
    tryCatch(
      {
        cn <- odbcDriverConnect(connection=sc_connection_string)
        curr_user = sqlQuery(cn, "SELECT CURRENT_USER", errors=TRUE)
        odbcClose(cn)
        curr_user
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
sc_da <- DataAccess_S3V2(connection_param=sc_connection_string, fs_id_param=sc_fs_id)
}
###################################################################################################
# END OF SOLVAS|CAPITAL HEADER - DO NOT MODIFY 
###################################################################################################
