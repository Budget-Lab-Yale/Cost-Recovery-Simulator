library(tidyverse)
library(data.table)
library(magrittr)

# User-supplied parameters 
runscript_id = 'baseline'

# Source all functions and define global variables
list.files('./src', recursive = T) %>% 
  walk(.f = ~ if (.x != 'main.R') source(file.path('./src/', .x)))

# Run simulation for all scenarios
scenario_output = map(runscript$id, do_scenario)

# TODO across-scenario post-processing comparisons
