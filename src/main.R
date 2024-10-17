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
walk(runscript$id, do_scenario)

# TODO across-scenario post-processing comparisons






actual = tibble(
  year = 2024:2033, 
  tf   =  c(-46.5, -40.6, 24.8, 17.5, 13.5, 9.0, 6.4, 3.9, 2.9, 2.3),
  jct  = c(-32.793, -35.465, -4.439, 26.031, 16.693, 11.773, 7.922, 4.267, 2.024, 0.9)
)





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




