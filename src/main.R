library(tidyverse)
library(data.table)
library(magrittr)

# User-supplied parameters 
runscript_id = 'baseline'


#---------------
# Configuration
#---------------

# Define functions
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
    
  # TODO Calculate depreciation deductions
  depreciation_deductions = calc_depreciation(investment)
  
  # TODO calculate deductions by year
  deductions_by_Year = deductions_by_year(depreciation_deductions)
}
