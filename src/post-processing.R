#---------------------------------------------------
# Post-processing.R
# 
# Contains functions to process detailed deductions
# file for summary output
#---------------------------------------------------


calc_recovery_ratios = function(scenario_info, depreciation_detailed, 
                                macro_projections, assumptions, group_var, 
                                spread = 0.02) {
  
  #----------------------------------------------------------------------------
  # Calculates the ratio of the value of depreciation deductions (both real  
  # and present value) to investment basis by year X specified grouping var.
  # 
  # Parameters:
  # - scenario_info       (list) : scenario info object 
  #                                (see get_scenario_info())
  # - depreciation_detailed (df) : tibble of deductions by asset class, long 
  #                                in investment year and wide in deduction 
  #                                year (see calc_all_depreciation()) 
  # - macro_projections     (df) : tibble of macro variable projections
  # - assumptions         (list) : assumptions object (see build_assumptions())
  # - group_var            (obj) : grouping variable
  # - spread               (dbl) : assumed discount rate spread over 10-year
  # 
  # Returns:
  # - void (writes output)
  #----------------------------------------------------------------------------
  
  # Generate file name dynamically 
  if (rlang::as_label(enquo(group_var)) == 'NULL') { 
    file_name = 'recovery_ratios.csv' 
  } else {
    file_name = paste0('recovery_ratios_', rlang::as_name(enquo(group_var)), '.csv')
  }
  
  
  depreciation_detailed %>% 
    
    # Join industry groups
    left_join(
      read_csv('./resources/industry_crosswalk.csv', show_col_types = FALSE) %>% 
        select(-standard_industry),
      by = 'industry'
    ) %>% 
    
    # Aggregate before reshaping long (for memory)
    group_by(year, {{ group_var }}) %>% 
    summarise(
      across(
        .cols = c(investment, matches('[[:digit:]]')), 
        .fns  = sum
      ), 
      .groups = 'drop'
    ) %>% 
    
    # Get investment year X deduction year totals
    pivot_longer(
      cols            = matches('[[:digit:]]'), 
      names_to        = 'deduction_year', 
      names_transform = as.integer,
      values_to       = 'deductions'
    ) %>% 
    filter(deduction_year >= year, deduction_year <= max(scenario_info$years)) %>% 
    
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
    group_by(year, {{ group_var }}) %>% 
    summarise(
      investment = unique(investment),
      real       = sum(deductions / cumprod(1 + lag(inflation_rate, default = 0))),
      pv         = sum(deductions / cumprod(1 + lag(discount_rate,  default = 0))), 
      .groups = 'drop'
    ) %>% 
    
    # Calculate lifetime recovery ratio
    mutate(across(.cols = c(real, pv), .fns = ~ . / investment)) %>% 
    select(-investment) %>% 
    write_csv(
      file.path(scenario_info$paths$output, 'totals', file_name)
    )
}

