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
  
  print(paste('Running scenario:', id))
  
  # Get scenario info
  scenario_info = get_scenario_info(id)
  
  # Build tax law and associated schedules 
  tax_law           = list()
  tax_law$params    = build_tax_law(scenario_info)
  tax_law$schedules = build_schedules(tax_law$params)
  
  # Build investment data
  investment = build_investment_data(scenario_info)
  
  # Read macro projections data
  macro_projections = build_macro_projections(scenario_info)
  
  # Calculate depreciation deductions by investment year and asset class
  deductions_detailed = calc_depreciation(investment, macro_projections, tax_law)
  
  # Post-processing
  by_deduction_year = get_by_deduction_year(deductions_detailed)
  #recovery_ratios   = calc_recovery_ratios(deductions_detailed, macro_projections, 0.02)
  
  # Return list of output tables 
  return(
    list(
      #deductions_detailed = deductions_detailed, 
      by_deduction_year   = by_deduction_year 
      #recovery_ratios     = recovery_ratios
    ) 
  )
}
