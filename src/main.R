library(tidyverse)
library(data.table)
library(magrittr)
library(yaml)
library(rlang)
library(ggtext)

# User-supplied parameters 
runscript_id = 'reforms/tcja_ext_options_temp'

# Source all functions and define global variables
list.files('./src', recursive = T) %>% 
  walk(.f = ~ if (.x != 'main.R') source(file.path('./src/', .x)))

# Run simulation for all scenarios
walk(runscript$id, do_scenario)

# Get stacked revenue estimates 
calc_stacked_revenue_estimates(runscript$id)
  