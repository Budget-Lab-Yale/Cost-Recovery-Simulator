#------------------------------------------------------------
# sim.R
#
# Contains functions to run simulation for a single scenario
#------------------------------------------------------------


do_scenario = function(id) {
  
  #----------------------------------------------------------------------------
  # Runs simulation for specified scenario.
  #
  # Parameters:
  # - id (str) : scenario ID
  # 
  # Returns: 
  # - list containing various detailed and summary tables (list)
  #----------------------------------------------------------------------------    
  
  # Get scenario info
  scenario_info = get_scenario_info(id)
  
  # Build investment data
  investment = build_investment_data(scenario_info)
  
  # Read macro projections
  macro_projections = c('historical.csv', 'projections.csv') %>% 
    map(.f = ~ file.path(scenario_info$paths$`Macro-Projections`, .x) %>% 
          read_csv(show_col_types = F)
    ) %>% 
    bind_rows()
  
  # PLACEHOLDER: FILL IN HISTORICAL INFLATION AND YIELDS (TK IN MACRO-PROJECTIONS)
  placeholder_series = tibble(year = 1970:2024) %>% 
    mutate(
      placeholder_cpiu    = 1.02 ^ (year - 1970), 
      placeholder_cpiu    = placeholder_cpiu / placeholder_cpiu[year == 2024], 
      placeholder_tsy_10y = 4
    ) %>% 
    filter(year < 2024)
  macro_projections %<>% 
    left_join(placeholder_series, by = 'year') %>% 
    mutate(
      cpiu    = if_else(year < 2024, placeholder_cpiu, cpiu), 
      tsy_10y = if_else(year < 2024, placeholder_tsy_10y, tsy_10y)
    )
  
  # Calculate depreciation deductions by investment year and asset class
  deductions_detailed = calc_all_depreciation(investment, macro_projections)
  
  # Post-processing
  by_deduction_year = get_by_deduction_year(deductions_detailed)
  recovery_ratios   = calc_recovery_ratios(deductions_detailed, macro_projections, 0.02)
  
  # Return list of output tables 
  return(
    list(
      deductions_detailed = deductions_detailed, 
      by_deduction_year   = by_deduction_year, 
      recovery_ratios     = recovery_ratios
    ) 
  )
}
