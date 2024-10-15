library(tidyverse)
library(data.table)
library(magrittr)
library(yaml)

# User-supplied parameters 
runscript_id = 'tests/expensing2023'

# Source all functions and define global variables
list.files('./src', recursive = T) %>% 
  walk(.f = ~ if (.x != 'main.R') source(file.path('./src/', .x)))

look = do_scenario('baseline')

# Run simulation for all scenarios
scenario_output = runscript$id %>% 
  map(do_scenario) %>% 
  set_names(runscript$id)

print(as.numeric(end-start))
# TODO across-scenario post-processing comparisons
