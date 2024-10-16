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
  
  print(paste('Running scenario:', id, ' '))
  
  # Get scenario info
  scenario_info = get_scenario_info(id)
  
  # Parse assumption parameters
  assumptions = build_assumptions(scenario_info) 
  
  # Build tax law and associated schedules 
  tax_law           = list()
  tax_law$params    = build_tax_law(scenario_info)
  tax_law$schedules = build_schedules(tax_law$params, assumptions$expensing_takeup)
  
  # Build investment data
  investment = build_investment_data(scenario_info, assumptions)
  
  # Read macro projections data
  macro_projections = build_macro_projections(scenario_info)
  
  # Calculate depreciation deductions by investment year and asset class
  deductions_detailed = calc_deductions(scenario_info, tax_law, macro_projections, investment, assumptions)
  
  # Post-processing
  get_by_deduction_year(scenario_info, deductions_detailed)
  calc_recovery_ratios(scenario_info, deductions_detailed, macro_projections, assumptions, NULL,        0.02)
  calc_recovery_ratios(scenario_info, deductions_detailed, macro_projections, assumptions, form,        0.02)
  calc_recovery_ratios(scenario_info, deductions_detailed, macro_projections, assumptions, asset_class, 0.02)
  calc_recovery_ratios(scenario_info, deductions_detailed, macro_projections, assumptions, industry,    0.02)
}
