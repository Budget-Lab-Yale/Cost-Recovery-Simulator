#----------------------------------------------------------
# configure.R
# 
# Functions to configure interface paths and parse tax law
#----------------------------------------------------------



get_scenario_info = function(id) { 
  
  #----------------------------------------------------------------------------
  # Parses runscript info for given scenario ID into list format.
  #
  # Parameters:
  #  - id (str) : scenario ID
  # 
  # Returns:
  # - list of runscript elements for specified scenario (list)
  #----------------------------------------------------------------------------
  
  # Get scenario info in list format
  scenario_info = runscript %>% 
    filter(id == id) %>% 
    unlist() %>% 
    as.list() 
  
  # Parse years
  years = scenario_info$years %>% 
    str_split_1(':') %>% 
    as.integer()
  scenario_info$years = years[1]:years[2]
  
  return(scenario_info)
  
}

