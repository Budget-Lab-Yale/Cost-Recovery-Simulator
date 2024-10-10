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
