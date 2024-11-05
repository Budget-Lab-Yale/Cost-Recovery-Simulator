library(tidyverse)
library(data.table)
library(magrittr)
library(yaml)
library(rlang)
library(ggtext)
library(tidyverse)

# Create output root
time_stamp  = file.path(format(Sys.time(), '%Y%m%d%H'))
dir.create(file.path(getwd(), "resources/output", time_stamp))

# User-supplied parameters 
runscript_id = 'tests/bonus'

# Source all functions and define global variables
list.files('./src', recursive = T) %>% 
  walk(.f = ~ if (.x != 'main.R' & .x != "main_DCS.R") source(file.path('./src/', .x)))

# Run simulation for all scenarios
walk(runscript$id, do_scenario, .progress = T)

across_scenario_calculating_and_graphing()

