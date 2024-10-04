library(yaml)
library(tidyverse)
library(data.table)
library(magrittr)


# Define functions
list.files('./src', recursive = T) %>% 
  walk(.f = ~ if (.x != 'main.R') source(file.path('./src/', .x)))

# TODO parse runsript / set globals
globals = list()

# TODO scenario loop
ids = c('baseline')
for (id in ids) {
  
  # Read investment projections
  investment = c('historical_data.csv', 'baseline_projections.csv') %>% 
    map(~ read_csv(file.path('./resources/input/baseline/', .x), show_col_types = F)) %>% 
    bind_rows() %>%
    pivot_longer(
      cols      = -year, 
      names_to  = 'asset_class', 
      values_to = 'investment'
    ) %>% 
    
    # Build and join tax law
    left_join(
      build_tax_law('baseline', max(.$year)), 
      by = c('year', 'asset_class')
    )
  
  temp = investment %>% filter(year > 1999 & !is.na(L))
  
  # TODO Calculate depreciation deductions
  depreciation_deductions = calc_depreciation(temp)
  
  # TODO calculate deductions by year
  deductions_by_Year = deductions_by_year(depreciation_deductions)
}
