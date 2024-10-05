library(tidyverse)
library(data.table)
library(magrittr)

# User-supplied parameters 
runscript_id = 'baseline'


#---------------
# Configuration
#---------------

# Source all functions
list.files('./src', recursive = T) %>% 
  walk(.f = ~ if (.x != 'main.R') source(file.path('./src/', .x)))

# Read runscript 
runscript = file.path('./config/runscripts/', paste0(runscript_id, '.csv')) %>% 
  read_csv(show_col_types = F)


#---------------
# Scenario loop
#---------------

ids = c('baseline')
for (id in ids) {
  
  # Get scenario info
  scenario_info = get_scenario_info(id)
  
  # Build investment data
  investment = build_investment_data(scenario_info)
  
  # TODO Read macro projections (PLACEHOLDER)
  macro_projections = read_csv('./resources/input/macro_projections.csv')
    
  # Calculate depreciation deductions
  by_asset_year = calc_all_depreciation(investment, macro_projections)
  
  # TODO calculate deductions by year
  by_year = deductions_by_year(by_asset_year)
}
