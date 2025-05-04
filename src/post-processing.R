#---------------------------------------------------
# Post-processing.R
# 
# Contains functions to process detailed deductions
# output and revenue files for supplementary output
#---------------------------------------------------




calc_revenue_estimate = function(scenario_info, revenue) {
  
  #----------------------------------------------------------------------------
  # Calculates the difference in revenues from baseline for a given non-
  # baseline scenario's revenue projections.
  # 
  # Parameters:
  # - scenario_info (list) : scenario info object (see get_scenario_info())
  # - revenue (df)         : scenario revenue levels
  #
  # Returns:
  # - tibble of revenue estimate (also writes output)
  #----------------------------------------------------------------------------
  
  revenue_estimate = output_root %>% 
    
    # Read baseline revenue
    file.path('baseline/totals/revenue.csv') %>% 
    read_csv(show_col_types = F) %>% 
    select(year, baseline = total) %>% 
    
    # Join counterfactual scenario revenue
    left_join(
      revenue %>% 
        select(year, reform = total), 
      by = 'year'
    ) %>% 
    
    # Calculate delta and write
    mutate(delta = reform - baseline) %>% 
    select(year, delta) %>%
    write_csv(
      file.path(scenario_info$paths$output, 'deltas/revenues.csv')
    ) %>% 
    return()
}



calc_stacked_revenue_estimates = function(scenarios) {
  
  #----------------------------------------------------------------------------
  # For each scenario listed in the runscript, calculates the marginal 
  # difference in revenues relative to the prior scenario in the list.
  # 
  # Parameters:
  # - scenarios (str[]) : list of scenario names in stacking order
  #
  # Returns: void (writes to final scenario's output folder)
  #----------------------------------------------------------------------------
  
  scenarios %>% 
    map(
      .f = ~ file.path(output_root, .x, 'deltas/revenues.csv') %>% 
        read_csv(show_col_types = F) %>% 
        mutate(scenario = .x, .before = everything())
    ) %>% 
    bind_rows() %>%
    group_by(year) %>%
    mutate(delta = delta - lag(delta)) %>%
    ungroup() %>% 
    pivot_wider(names_from = year, values_from = delta) %>% 
    filter(scenario != 'baseline') %>% 
    write_csv(file.path(output_root, scenarios[length(scenarios)], 'deltas/stacked_revenues.csv'))
}



calc_recovery_ratios = function(scenario_info, depreciation_detailed, 
                                macro_projections, assumptions, group_var, 
                                spread) {
  
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
    write_csv(
      file.path(scenario_info$paths$output, 'totals', file_name)
    )
}




# convert deductions to tax liability
# calc PV of tax liability change, discounting by growth in deductions 
#   (is this not just revenue?)
# 

# idea: treasury's long and short run are very similiar if investment grows at discount rate and doesnt chage a lot. test this.
# 
# - just one year PV 
# - net interest
# - net