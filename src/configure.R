#----------------------------------------------------------
# configure.R
# 
# Functions and operations to configure file paths and 
# parse tax law
#----------------------------------------------------------

# Read runscript 
runscript = file.path('./config/runscripts/', paste0(runscript_id, '.csv')) %>% 
  read_csv(show_col_types = F)

# Instantiate global file paths
platform_paths = read_csv('./config/platform_paths.csv', show_col_types = T)

# Create output paths
# TODO


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

