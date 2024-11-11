library(tidyverse)
library(data.table)
library(magrittr)
library(yaml)
library(rlang)
library(ggtext)

# User-supplied parameters 
runscript_id = 'reforms/options'

# Source all functions and define global variables
list.files('./src', recursive = T) %>% 
  walk(.f = ~ if (.x != 'main.R') source(file.path('./src/', .x)))

# Run simulation for all scenarios
walk(runscript$id, do_scenario)

# Run across-scenario post-processing comparisons
across_scenario_calculating_and_graphing()




c('baseline', 'wyden-smith') %>% 
  map(.f = ~ read_csv(file.path(output_root, .x,  'totals/revenue.csv')) %>%
        mutate(scenario = .x, total = pt + ccorp)) %>% 
  bind_rows() %>% 
  pivot_longer(cols = c(ccorp, pt, total)) %>% 
  pivot_wider(names_from = scenario) %>% 
  mutate(across(.cols = -c(year, name, baseline), .fns = ~ . - baseline)) %>% 
  select(-baseline) %>% 
  filter(year >= 2023, year <= 2042, name == 'total') %>% 
  group_by(name) %>% 
  mutate(`wyden-smith` = if_else(year == 2024, `wyden-smith` + `wyden-smith`[year == 2023], `wyden-smith`)) %>%
  mutate(`wyden-smith` = if_else(year == 2023, NA, `wyden-smith`)) %>% 
  ungroup() %>% 
  rename(tbl = `wyden-smith`) %>% 
  left_join(actual, by = 'year') %>% 
  filter(year %in% 2024:2033) %>%
  pivot_longer(cols = -c(year, name), names_to = 'source') %>% 
  ggplot(aes(x = year, y = value, colour = source)) + 
  geom_line()+ 
  geom_point(size = 4) + 
  geom_hline(yintercept = 0)




