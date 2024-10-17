#----------------------------------------------------------
# calc.R
# 
# Contains functions to calculate depreciation deductions
# given investment projections and tax law parameters
#----------------------------------------------------------


calc_depreciation = function(scenario_info, tax_law, macro_projections, investment, assumptions) {
  
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
      
      # ...and associated schedule of depreciation deductions
      expand_grid(t = 1:max(tax_law$schedules$t)) %>% 
      left_join(tax_law$schedules, by = c('B', 'L', 'bonus', 's179', 'bonus_takeup', 's179_takeup', 't')) %>% 
      
      # Calculate depreciation deductions
      mutate(depreciation = investment* share_depreciated) %>% 
    
      # Adjust for inflation/time value if specified
      mutate(deduction_year = year + t - 1) %>%
      left_join(index_adjustment, by = c('deduction_year', 'indexation')) %>% 
      mutate(depreciation = depreciation * index_factor) %>%
          
      # Reshape wide in deduction year (saves memory)
      select(year, form, asset_class, industry, L, investment, deduction_year, depreciation) %>% 
      pivot_wider(
        names_from  = deduction_year, 
        values_from = depreciation
      ) %>% 
      return()
  }
   
  # Bind years together, replace NAs (no deduction that year) with 0s, and write
  output %<>% 
    bind_rows() %>%
    mutate(across(.cols = everything(), .fns = ~ replace_na(., 0))) %>% 
    write_csv(file.path(scenario_info$paths$output, 'detail', 'detail.csv'))
    
  return(output)
}



calc_deduction_usage = function(scenario_info, depreciation_detailed, assumptions) {
  
  #----------------------------------------------------------------------------
  # Calculates gross depreciation by year, then allocates those deductions 
  # between usable depreciation deductions and NOLs which are taken in some 
  # pattern specified by assumptions. Writes output. 
  #
  # Parameters:
  #  - scenario_info       (list) : scenario info object 
  #                                 (see get_scenario_info())
  #  - depreciation_detailed (df) : tibble of deductions by asset class, long 
  #                                 in investment year and wide in deduction 
  #                                 year (see calc_all_depreciation()) 
  #  - assumptions         (list) : assumptions object 
  #                                 (see build_assumptions())
  # 
  # Returns:
  #  - tibble with depreciation and NOL deductions (df)
  #----------------------------------------------------------------------------
  
  # First, depreciation:
  depreciation = depreciation_detailed %>%
    
    # Aggregate in wide format (helps with RAM issues)
    group_by(form) %>% 
    summarise(
      across(
        .cols = matches('[[:digit:]]'), 
        .fns  = sum
      ), 
      .groups = 'drop'
    ) %>%
    
    # Reshape long in deduction year
    pivot_longer(
      cols            = -form, 
      names_to        = 'year', 
      names_transform = as.integer, 
      values_to       = 'gross_depreciation'
    ) 
  
  
  # Net, net operating losses
  nols = depreciation %>% 

    # Split gross deductions between used depreciation and NOLs
    left_join(assumptions$usage,        by = c('form', 'year')) %>% 
    left_join(assumptions$nol_schedule, by = c('form', 'year')) %>% 
    mutate(
      across(
        .cols = starts_with('t'), 
        .fns  = ~ . * gross_depreciation * (1 - share_used)
      )
    ) %>% 
    
    # Reshape long in NOL year
    select(-share_used) %>% 
    pivot_longer(
      cols            = starts_with('t'), 
      names_prefix    = 't', 
      names_transform = as.integer,
      names_to        = 't',
      values_to       = 'nol'
    ) %>% 
    mutate(nol_year = year + t - 1) %>% 
    
    # Aggregate 
    group_by(form, year = nol_year) %>% 
    summarise(nol = sum(nol), .groups = 'drop')
  
  
  # Combine, write, and return
  output = depreciation %>% 
    left_join(assumptions$usage, by = c('form', 'year')) %>% 
    mutate(depreciation = gross_depreciation * share_used) %>% 
    left_join(nols, by = c('form', 'year')) %>% 
    select(form, year, depreciation, nol) %>% 
    mutate(total = depreciation + nol) %>% 
    write_csv(file.path(scenario_info$paths$output, 'totals/deductions.csv'))
  
    return(output)
}



calc_revenue = function(scenario_info, tax_law, deductions) {
  
  #----------------------------------------------------------------------------
  # Given projected path of deductions and tax rates, calculates implied 
  # revenue loss associated with depreciation policy. Writes output. 
  #
  # Parameters:
  #  - scenario_info (list) : scenario info object (see get_scenario_info())
  #  - tax_law       (list) : tax law object (see build_tax_law())
  #  - deductions      (df) : tibble of deductions by legal form and year
  # 
  # Returns:
  #  - tibble with revenue loss by legal form and year (df)
  #----------------------------------------------------------------------------
  
  output = deductions %>% 
    left_join(
      tax_law$params %>% 
        distinct(form, year, tax_rate),
      by = c('form', 'year')
    ) %>% 
    mutate(value = -total * tax_rate) %>% 
    select(form, year, value) %>% 
    pivot_wider(names_from = form) %>% 
    mutate(total = ccorp + pt) %>% 
    write_csv(file.path(scenario_info$paths$output, 'totals/revenue.csv'))
  
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



