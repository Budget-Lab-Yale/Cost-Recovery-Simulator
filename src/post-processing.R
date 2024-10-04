
calc_rate_schedule = function() {
  
  # get the b's and L's from the config file
  
  horizon = 20
  
  1:nrow(pairs) %>%
    map(.f = ~ apply_deduction(1, pairs[.x, "b"], pairs[.x, "L"]) %>%
          as_tibble() %>%
          rowid_to_column() %>%
          pivot_wider(names_from = rowid, values_from = value) %>% 
          fill_years(., 1:horizon) %>%
          mutate(schedule = pairs[.x, "schedule"]) %>%
          pivot_longer(!schedule, names_to = 'years', values_to = 'depreciation') %>%
          arrange(years) %>%
          pivot_wider(names_from = years, values_from = depreciation) %>%
          rename(across(!c()))
        ) %>%
        bind_rows() %>%
        return()
}

deductions_by_year = function(depreciation) {
  
  depreciation %>%
    pivot_longer(!c(year, schedule), names_to = 'Deduction_Year', values_to = 'Deduction') %>%
    group_by(Deduction_Year) %>%
    reframe(
      Deduction = sum(Deduction)
    ) %>%
    return()
}
