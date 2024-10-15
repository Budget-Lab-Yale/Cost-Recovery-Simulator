#----------------------------------------------------------
# configure.R
# 
# Functions and operations to configure file paths and 
# parse tax law
#----------------------------------------------------------

# Set model version 
version = 1

# Read runscript 
runscript = file.path('./config/runscripts/', paste0(runscript_id, '.csv')) %>% 
  read_csv(show_col_types = F)

# Instantiate global file roots
platform_defaults = read_yaml('./config/platform_defaults.yaml')

# Create output root
time_stamp  = file.path(format(Sys.time(), '%Y%m%d%H'))
output_root = file.path(platform_defaults$roots$output, time_stamp)
dir.create(output_root)


get_scenario_info = function(scenario_id) { 
  
  #----------------------------------------------------------------------------
  # Parses runscript info for given scenario ID into list format.
  #
  # Parameters:
  #  - scenario_id (str) : scenario ID
  # 
  # Returns:
  # - list of runscript elements for specified scenario (list)
  #----------------------------------------------------------------------------
  
  # Get scenario info in list format
  scenario_info = runscript %>% 
    filter(id == scenario_id) %>% 
    unlist() %>% 
    as.list()
  
  # Parse years
  years = scenario_info$years %>% 
    str_split_1(':') %>% 
    as.integer()
  scenario_info$years = years[1]:years[2]
  
  # Create data dependency paths
  scenario_info$paths = list()
  scenario_info$paths$`Investment-Projections` = file.path(
    platform_defaults$roots$input, 
    ifelse(is.null(scenario_info$`Investment-Projections`), 
           platform_defaults$dependencies$`Investment-Projections`, 
           scenario_info$`Investment-Projections`)
  )
  scenario_info$paths$`Macro-Projections` = file.path(
    platform_defaults$roots$input, 
    ifelse(is.null(scenario_info$`Macro-Projections`), 
           platform_defaults$dependencies$`Macro-Projections`, 
           scenario_info$`Macro-Projections`)
  )
  scenario_info$`Investment-Projections` = NULL
  scenario_info$`Macro-Projections`      = NULL
  
  # Create output paths
  scenario_info$paths$output = file.path(output_root, scenario_id)
  dir.create(scenario_info$paths$output)
  dir.create(file.path(scenario_info$paths$output, 'detail'))
  dir.create(file.path(scenario_info$paths$output, 'totals'))
  dir.create(file.path(scenario_info$paths$output, 'deltas'))
  
  return(scenario_info)
}

