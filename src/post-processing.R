#---------------------------------------------------
# Post-processing.R
# 
# Contains functions to process detailed deductions
# file for summary output
#---------------------------------------------------


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
  #  - tibble of deductions by deduction year, wide in legal form (df) 
  #----------------------------------------------------------------------------
  
  deductions_detailed %>%
    pivot_longer(
      cols            = -c(form, year, asset_class, industry, investment), 
      names_to        = 'deduction_year', 
      names_transform = as.integer,
      values_to       = 'deductions'
    ) %>%
    group_by(form, deduction_year) %>%
    summarise(value = sum(deductions), .groups = 'drop') %>%
    pivot_wider(names_from = form) %>% 
    return()
}



calc_recovery_ratios = function(deductions_detailed, macro_projections, spread = 0.02) {
  
  #----------------------------------------------------------------------------
  # Calculates the ratio of the value of depreciation deductions (both real  
  # and present value) to investment basis by form X asset class X industry X 
  # year.
  # 
  # Parameters:
  # - deductions_detailed (df) : tibble of deductions by asset class, long in
  #                              investment year and wide in deduction year 
  #                              (see calc_all_depreciation()) 
  # - macro_projections   (df) : tibble of macro variable projections
  # - spread             (dbl) : assumed discount rate spread over 10-year
  # 
  # Returns:
  # - list containing summary metrics of recovery ratios for different levels 
  #   of aggregation (list)
  #----------------------------------------------------------------------------
  
  detailed = deductions_detailed %>% 
    
    # Get investment year X deduction year totals
    pivot_longer(
      cols            = -c(form, year, asset_class, industry, investment), 
      names_to        = 'deduction_year', 
      names_transform = as.integer,
      values_to       = 'deductions'
    ) %>% 
    filter(deduction_year >= year) %>% 
    
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
    group_by(form, year, asset_class, industry) %>% 
    summarise(
      real = sum(deductions / cumprod(1 + lag(inflation_rate, default = 0))),
      pv   = sum(deductions / cumprod(1 + lag(discount_rate,  default = 0))), 
      .groups = 'drop'
    ) %>% 
    
    # Calculate lifetime recovery ratio
    left_join(
      deductions_detailed %>% 
        group_by(form, year, asset_class, industry) %>% 
        summarise(investment = sum(investment), .groups = 'drop'), 
      by = c('form', 'year', 'asset_class', 'industry')
    ) %>% 
    select(form, year, asset_class, industry, investment, real, pv) %>%  
    mutate(across(.cols = c(real, pv), .fns = ~ . / investment))
    
  # Return various summary metrics
  get_summary = function(group_var) { 
    detailed %>%
      group_by(year, {{ group_var }}) %>% 
      summarise(
        across(
          .cols = c(real, pv), 
          .fns  = ~ weighted.mean(., investment)
        ), 
        .groups = 'drop'
      ) %>% 
      return()
  }
  return(
    list(
      overall        = get_summary(),
      by_form        = get_summary(form),
      by_asset_class = get_summary(asset_class),
      by_industry    = get_summary(industry)
    )
  )
}
  

