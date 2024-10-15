#----------------------------------------------------------
# calc.R
# 
# Contains functions to calculate depreciation deductions
# given investment projections and tax law parameters
#----------------------------------------------------------


calc_depreciation = function(scenario_info, investment, macro_projections, tax_law) {
  
  #----------------------------------------------------------------------------
  # Calculates depreciation deductions for all projected investment for all
  # years. Iterates over year to deal with RAM limitations. 
  #
  # Parameters:
  #  - scenario_info   (list) : scenario info object (see get_scenario_info())
  #  - investment        (df) : detailed investment (see build_investment())
  #  - macro_projections (df) : macroeconomic data
  #  - tax_law          (lst) : list containing tax law params and schedules
  # 
  # Returns:
  #  - tibble with deductions attached, wide in deduction year (df)
  #----------------------------------------------------------------------------
  
  # Parse indexation values
  indexes = macro_projections %>% 
    mutate(
      inflation = cpiu / lag(cpiu) - 1, 
      timevalue = tsy_10y / 100,
      none      = 0
    ) %>% 
    select(year, inflation, timevalue, none)
  
  # For each investment year...
  output = list()
  for (yr in unique(investment$year)) {
    
    # Calculate indexation adjustment factors
    index_adjustment = indexes %>% 
      filter(year >= yr) %>%
      rename(deduction_year = year) %>%
      mutate(
        across(
          .cols = -deduction_year, 
          .fns  = ~ cumprod(1 + lag(., default = 0))
        )
      ) %>% 
      pivot_longer(
        cols      = -deduction_year, 
        names_to  = 'indexation', 
        values_to = 'factor'
      )
    
    # Filter to this year only
    output[[length(output) + 1]] = investment %>% 
      filter(year == yr) %>% 
      
      # Join tax law parameters and associated depreciation schedules 
      left_join(tax_law$params, by = c('form', 'year', 'asset_class', 'industry')) %>% 
      expand_grid(t = 1:max(tax_law$schedules$t)) %>% 
      left_join(tax_law$schedules, by = c('B', 'L', 'bonus', 's179', 't')) %>% 
      
      # Calculate depreciation deductions
      filter(share > 0) %>%
      mutate(deduction = investment * share) %>% 
      
      # Adjust for inflation/time value if specified
      mutate(deduction_year = year + t - 1) %>%
      left_join(index_adjustment, by = c('deduction_year', 'indexation')) %>% 
      mutate(deduction = deduction * factor) %>% 
          
      # Reshape wide in deduction year(saves memory)
      select(year, form, asset_class, industry, L, investment, deduction_year, deduction) %>% 
      pivot_wider(
        names_from  = deduction_year, 
        values_from = deduction
      )
  }
   
  # Bind years together, replace NAs (no deduction that year) with 0s, and write
  output %<>% 
    bind_rows() %>%
    mutate(across(.cols = everything(), .fns = ~ replace_na(., 0))) %>% 
    write_csv(file.path(scenario_info$paths$output, 'detail', 'detail.csv'))
    
  return(output)
}



calc_schedule = function(B, L, bonus, s179, max_t) {
  
  #----------------------------------------------------------------------------
  # Calculates schedule of deductions give MACRS, bonus, and 179 params.
  #
  # Parameters:
  #  - B     (dbl) : decay rate factor
  #  - L     (dbl) : cost recovery life
  #  - bonus (dbl) : bonus depreciation rate
  #  - s179  (dbl) : share of investment eligible for section 179 expensing
  #  - max_t (dbl) : number of years for which to show series 
  # 
  # Returns:
  #  - tibble of schedule with parameters, long in relative year (df)
  #----------------------------------------------------------------------------
  
  # Calculate amount avilable for immediate expensing
  expensed = s179 + bonus * (1 - s179)
  
  # Calculate remaining MACRS schedule and add in expensed amount
  schedule    = (1 - expensed) * calc_macrs(B, L)
  schedule[1] = schedule[1] + expensed

  # Return output in df format
  tibble(
    t     = 1:ceiling(max_t), 
    share = c(schedule, rep(0, ceiling(max_t) - length(schedule)))
  ) %>% 
  mutate(B = B, L = L, bonus = bonus, s179 = s179, .before = everything()) %>% 
  return()

}



calc_macrs = function(B, L) {
  
  #----------------------------------------------------------------------------
  # Calculates MACRS schedule given decay rate and useful life params.
  #
  # Parameters:
  #  - B (dbl) : decay rate factor
  #  - L (dbl) : cost recovery life
  # 
  # Returns:
  #  - vector of deductions (dbl[])
  #----------------------------------------------------------------------------
  
  # Calculate half-year DB schedule 
  db_bal   = cumprod(rep(1 - B / L, L))
  db_whole = lag(db_bal, default = 1) - db_bal 
  db_half  = c(db_whole / 2, 0) + c(0, db_whole / 2)
  
  # Calculate half-year SL schedule
  sl_whole = rep(1 / L, L)
  sl_half  = c(sl_whole / 2, 0) + c(0, sl_whole / 2)
  
  # Stop if SL and DB are the same
  if (all(sl_half == db_half)) {
    return(sl_half)  
  }
  
  # Determine period in which SL becomes more generous -- and remaining balance at that point
  switch = min(which(sl_half > db_half)) + if_else(L == 10, 1, 0)  # no idea why 10 year is slightly different than every other class
  remaining_bal = 1 - sum(db_half[1:switch])
  
  # Calculate remaining SL schedule
  remaining_t  = L - switch + 0.5
  sl_remaining = (remaining_bal / remaining_t) / c(rep(1, remaining_t), 2)

  # Piece together and return
  return(c(db_half[1:switch], sl_remaining))
}


