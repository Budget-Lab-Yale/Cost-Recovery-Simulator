#---------------------------------------------------------
# Contains the code to generate the data presented in the 
# depreciation reform options report
#---------------------------------------------------------


output_root = './resources/output/2024120415'
macro_projections = build_macro_projections(get_scenario_info('baseline'))

# Read revenue estimates and calculate share-of-GDP 
revenue_estimates = runscript$id %>% 
  map(.f = ~ read_csv(file.path(output_root, .x,  'totals/revenue.csv'), show_col_types = F) %>%
        mutate(name = .x) %>% 
        select(name, year, value = total)) %>% 
  bind_rows() %>% 
  pivot_wider() %>% 
  mutate(across(.cols = -c(year, baseline), .fns = ~ . - baseline)) %>% 
  select(-baseline) %>% 
  filter(year >= 2025, year <= 2054) %>%
  ungroup() %>% 
  pivot_longer(cols = -year, names_to = 'scenario', values_to = 'nominal') %>% 
  left_join(macro_projections %>% select(year, gdp_fy), by = 'year') %>% 
  mutate(share_gdp = 100 * nominal / gdp_fy) 


# Calculate budget-window totals
totals = revenue_estimates %>% 
  group_by(
    scenario,
    decade = case_when(
      year <= 2034 ~ 1, 
      year <= 2044 ~ 2, 
      year <= 2054 ~ 3
    )
  ) %>% 
  summarise(
    `Nominal dollars` = round(sum(nominal)), 
    `Share of GDP (percentage points)` = round(weighted.mean(share_gdp, gdp_fy), 2), 
    .groups = 'drop'
  )

# Write totals table
totals %>% 
  pivot_longer(cols = -c(scenario, decade), names_to = 'Series') %>% 
  pivot_wider(names_from = decade) %>% 
  arrange(Series, scenario) %>% 
  write.csv()

# Write revenue estimates
revenue_estimates %>% 
  select(-gdp_fy) %>% 
  arrange(scenario, year) %>% 
  write.csv()

# Calculate recovery ratios by industry
recovery_ratios = c('baseline', 'perm_bonus_2025', 'perm_expensing_2025') %>% 
  map(.f = ~ read_csv(file.path(output_root, .x,  'totals/recovery_ratios_major_industry.csv'), show_col_types = F) %>% 
        mutate(scenario = .x)) %>% 
  bind_rows() %>% 
  filter(year == 2027) %>% 
  select(-real) %>% 
  pivot_wider(names_from = scenario, values_from = pv) %>% 
  mutate(
    perm_bonus_2025 = perm_bonus_2025 - baseline, 
    perm_expensing_2025 = perm_expensing_2025 - (perm_bonus_2025 + baseline)
  ) %>% 
  select(year, Industry = major_industry, MACRS = baseline, `100% bonus` = perm_bonus_2025, ` Full expensing` = perm_expensing_2025) %>% 
  mutate(Industry = fct_reorder(Industry, -MACRS)) %>% 
  pivot_longer(cols = -c(year, Industry), names_to = 'Scenario') 

# Write recovery ratios
recovery_ratios %>% 
  select(-year) %>% 
  pivot_wider(names_from = Scenario) %>%
  arrange(MACRS) %>% 
  write.csv()

# Recovery ratio chart
recovery_ratios %>% 
  ggplot(aes(x = Industry, y = value, fill = Scenario)) + 
  geom_col() + 
  scale_y_continuous(labels = scales::label_percent()) + 
  labs(y = 'Cost recovery ratio') + 
  coord_flip() + 
  ggtitle('Cost recovery ratio by industry')


# Example chart code
revenue_estimates %>% 
  filter(scenario == 'ncr_2025') %>% 
  pivot_longer(cols = c(nominal, share_gdp), names_to = 'metric') %>% 
  mutate(metric = if_else(metric == 'nominal', 'Nominal dollars', 'Share of GDP (percentage points)')) %>%
  left_join(totals %>% 
              mutate(
                year = case_when(
                  decade == 1 ~ 2029, 
                  decade == 2 ~ 2039, 
                  decade == 3 ~ 2049
                )
              ) %>% 
              pivot_longer(cols = contains(' '), names_to = 'metric', values_to = 'total') %>% 
              mutate(label = paste0('Decade ', decade, ': ', total)), 
            by = c('year', 'scenario', 'metric')
  ) %>%  
  group_by(metric) %>% 
  mutate(new_y = if_else(metric == 'Nominal dollars', 1, 0.02)) %>% 
  ggplot(aes(x = year, y = value)) + 
  geom_col() + 
  geom_hline(yintercept = 0) + 
  geom_label(aes(y = new_y, label = label)) + 
  facet_wrap(~metric, scales = 'free') + 
  theme_bw() + 
  labs(x = element_blank(), y = element_blank()) + 
  scale_x_continuous(breaks = seq(2025, 2055, 5))
