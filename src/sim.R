
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
  
  # TODO Read macro projections (PLACEHOLDER)
  macro_projections = read_csv('./resources/input/macro_projections.csv')
  
  # Calculate depreciation deductions by investment year and asset class
  deductions_detailed = calc_all_depreciation(investment, macro_projections)
  
  # Tabulate deductions by deduction year
  by_deduction_year = get_by_deduction_year(deductions_detailed)
  
  # Return list of output tables 
  return(
    list(
      deductions_detailed, 
      by_deduction_year
    ) 
  )
  
}
