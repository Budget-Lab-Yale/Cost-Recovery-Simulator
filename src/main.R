library(tidyverse)
library(data.table)
library(magrittr)

# User-supplied parameters 
runscript_id = 'baseline'


#---------------
# Configuration
#---------------

# Source all functions and define global variables
list.files('./src', recursive = T) %>% 
  walk(.f = ~ if (.x != 'main.R') source(file.path('./src/', .x)))

# Read runscript 
runscript = file.path('./config/runscripts/', paste0(runscript_id, '.csv')) %>% 
  read_csv(show_col_types = F)


#------------
# Simulation 
#------------

# Run simulation for all scenarios
scenario_output = map(runscript$id, do_scenario)


#-----------------
# Post-processing
#-----------------

# TODO across-scenario comparisons
