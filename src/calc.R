#----------------------------------------------------------
# calc.R
# 
# Contains functions to calculate depreciation deductions
# given investment projections and tax law parameters
#----------------------------------------------------------


calc_deductions = function(scenario_info, tax_law, macro_projections, investment, assumptions) {
  
  #----------------------------------------------------------------------------
  # Calculates depreciation deductions for all projected investment for all
  # years. Iterates over years to deal with RAM limitations. 
  #
  # Parameters:
  #  - scenario_info   (list) : scenario info object (see get_scenario_info())
  #  - tax_law         (list) : list containing tax law params and schedules
  #  - macro_projections (df) : macroeconomic data
  #  - investment        (df) : detailed investment (see build_investment())
  #  - assumptions     (list) : assumptions object (see build_assumptions())
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
        values_to = 'index_factor'
      )
    
    # Filter to this year only
    output[[length(output) + 1]] = investment %>% 
      filter(year == yr) %>% 
      
      # Join: tax law parameters...  
      left_join(tax_law$params, by = c('form', 'year', 'asset_class', 'industry')) %>% 
      
      # ...and expensing takeup assumptions...
      left_join(assumptions$expensing_takeup, by = c('form', 'year')) %>%
      
      # ...and associated schedule of depreciation deductions...
      expand_grid(t = 1:max(tax_law$schedules$t)) %>% 
      left_join(tax_law$schedules, by = c('B', 'L', 'bonus', 's179', 'bonus_takeup', 's179_takeup', 't')) %>% 
      
      # ...and year-1 usage rates, which we use to adjust the fraction actually deducted...
      left_join(assumptions$year1_usage, by = c('form', 'year')) %>% 
      
      # ...and, finally, the schedule of net operating loss usage for unused depreciation deductions
      left_join(
        assumptions$nol_schedule %>% 
          pivot_longer(
            cols            = starts_with('t'), 
            names_prefix    = 't', 
            names_transform = as.integer, 
            names_to        = 't', 
            values_to       = 'share_nol'
          ),
        by = c('form', 'year', 't')
      ) %>% 
      mutate(share_nol = replace_na(share_nol, 0)) %>% 
      
      # Calculate depreciation and NOL deductions
      mutate(
        depreciation = investment * share_used       * share_depreciated,
        nol          = investment * (1 - share_used) * share_nol
      ) %>% 
      
      # Adjust for inflation/time value if specified
      mutate(deduction_year = year + t - 1) %>%
      left_join(index_adjustment, by = c('deduction_year', 'indexation')) %>% 
      mutate(across(.cols = c(depreciation, nol), .fns = ~ . * index_factor)) %>%
          
      # Reshape wide in deduction year (saves memory)
      select(year, form, asset_class, industry, L, investment, deduction_year, depreciation, nol) %>% 
      pivot_longer(cols = c(depreciation, nol), names_to = 'deduction_type') %>% 
      pivot_wider(names_from = deduction_year)
  }
   
  # Bind years together, replace NAs (no deduction that year) with 0s, and write
  output %<>% 
    bind_rows() %>%
    mutate(across(.cols = everything(), .fns = ~ replace_na(., 0))) %>% 
    write_csv(file.path(scenario_info$paths$output, 'detail', 'detail.csv'))
    
  return(output)
}



calc_schedule = function(B, L, bonus, s179, bonus_takeup, s179_takeup, max_t) {
  
  #----------------------------------------------------------------------------
  # Calculates schedule of deductions give MACRS, bonus, and 179 params.
  #
  # Parameters:
  #  - B            (dbl) : decay rate factor
  #  - L            (dbl) : cost recovery life
  #  - bonus        (dbl) : bonus depreciation rate
  #  - s179         (dbl) : share of investment eligible for 179 expensing
  #  - bonus_takuup (dbl) : share of eligible bonus investment used
  #  - s179_takeup  (dbl) : share of eligible 179 investment used 
  #  - max_t        (dbl) : number of years for which to show series 
  # 
  # Returns:
  #  - tibble of schedule with parameters, long in relative year (df)
  #----------------------------------------------------------------------------
  
  # Calculate amount of investment immediately expensing
  bonus_used = bonus * bonus_takeup
  s179_used  = s179  * s179_takeup
  expensed   = s179_used + bonus_used * (1 - s179_used)
  
  # Calculate remaining MACRS schedule and add in expensed amount
  schedule    = (1 - expensed) * calc_macrs(B, L)
  schedule[1] = schedule[1] + expensed

  # Return output in df format
  tibble(
    t = 1:ceiling(max_t), 
    share_depreciated = c(schedule, rep(0, ceiling(max_t) - length(schedule)))
  ) %>% 
  mutate(
    B            = B, 
    L            = L, 
    bonus        = bonus, 
    s179         = s179, 
    bonus_takeup = bonus_takeup, 
    s179_takeup  = s179_takeup, 
    .before = everything()
  ) %>% 
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
  switch = min(which(sl_half > db_half)) + if_else(B > 1 & L == 10, 1, 0)  # no idea why 10 year is slightly different than every other class
  remaining_bal = 1 - sum(db_half[1:switch])
  
  # Calculate remaining SL schedule
  remaining_t  = L - switch + 0.5
  sl_remaining = (remaining_bal / remaining_t) / c(rep(1, remaining_t), 2)

  # Piece together and return
  return(c(db_half[1:switch], sl_remaining))
}



