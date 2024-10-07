


get_by_deduction_year = function(deductions_detailed) {
  
  #----------------------------------------------------------------------------
  # Tabulates total deductions by deduction year.
  # 
  # Parameters:
  #  - deductions_detailed (df) : tibble of deductions by asset class, long in
  #                               investment year and wide in deduction year 
  #                               (see calc_all_depreciation()) 
  #
  # Returns:
  #  - long tibble of deductions by deduction year (df) 
  #----------------------------------------------------------------------------
  
  deductions_detailed %>%
    pivot_longer(
      cols      = -c(year, asset_class, investment), 
      names_to  = 'deduction_year', 
      values_to = 'deductions'
    ) %>%
    group_by(deduction_year) %>%
    summarise(deductions = sum(deductions)) %>%
    return()
}




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



