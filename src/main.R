library(tidyverse)
library(data.table)
library(magrittr)
library(yaml)

# User-supplied parameters 
runscript_id = 'tests/bonus'

# Source all functions and define global variables
list.files('./src', recursive = T) %>% 
  walk(.f = ~ if (.x != 'main.R') source(file.path('./src/', .x)))

# Run simulation for all scenarios
walk(runscript$id, do_scenario, .progress = T)

# TODO across-scenario post-processing comparisons



