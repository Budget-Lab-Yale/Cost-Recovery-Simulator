


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
      cols            = -c(year, asset_class, investment), 
      names_to        = 'deduction_year', 
      names_transform = as.integer,
      values_to       = 'deductions'
    ) %>%
    group_by(deduction_year) %>%
    summarise(deductions = sum(deductions)) %>%
    return()
}



calc_recovery_ratios = function(deductions_detailed, macro_projections, spread = 0.02) {
  
  #----------------------------------------------------------------------------
  # Calculates the ratio of the value of depreciation deductions (both real  
  # and present value) to investment basis for each year and asset class.  
  # 
  # Parameters:
  # - deductions_detailed (df) : tibble of deductions by asset class, long in
  #                              investment year and wide in deduction year 
  #                              (see calc_all_depreciation()) 
  # - macro_projections   (df) : tibble of macro variable projections
  # - spread             (dbl) : assumed discount rate spread over 10-year
  #----------------------------------------------------------------------------
  
  deductions_detailed %>% 
    
    # Get investment year X deduction year totals
    pivot_longer(
      cols            = -c(year, asset_class, investment), 
      names_to        = 'deduction_year', 
      names_transform = as.integer,
      values_to       = 'deductions'
    ) %>% 
    filter(deduction_year >= year) %>% 
    group_by(asset_class, year, deduction_year) %>% 
    summarise(
      deductions = sum(deductions), 
      .groups = 'drop'
    ) %>% 
    
    # Join risk-free rate and calculate discount rate
    left_join(
      macro_projections %>% 
        mutate(
          inflation_rate = cpiu / lag(cpiu) - 1, 
          discount_rate  = tsy_10y / 100 + spread
        ) %>% 
        select(year, inflation_rate, discount_rate), 
      by = c('deduction_year' = 'year')
    ) %>% 
    
    # Calculate present value of deductions
    group_by(asset_class, year) %>% 
    summarise(
      real = sum(deductions / cumprod(1 + lag(inflation_rate, default = 0))),
      pv   = sum(deductions / cumprod(1 + lag(discount_rate,  default = 0))), 
      .groups = 'drop'
    ) %>% 
    
    # Calculate lifetime recovery ratio
    left_join(
      deductions_detailed %>% 
        group_by(asset_class, year) %>% 
        summarise(investment = sum(investment), .groups = 'drop'), 
      by = c('asset_class', 'year')
    ) %>% 
    mutate(across(.cols = c(real, pv), .fns = ~ . / investment)) %>% 
    
    # Calculate total investment-weighted average and append 
    bind_rows(
      (.) %>% 
        group_by(year) %>% 
        summarise(across(.cols = c(real, pv), .fns  = ~ weighted.mean(., investment))) %>% 
        mutate(asset_class = '-TOTAL-')
    ) %>% 
    arrange(year, asset_class) %>% 
    select(year, asset_class, real, pv) %>% 
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



