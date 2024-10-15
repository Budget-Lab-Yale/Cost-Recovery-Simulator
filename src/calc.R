#----------------------------------------------------------
# calc.R
# 
# Contains functions to calculate depreciation deductions
# given investment projections and tax law parameters
#----------------------------------------------------------


calc_depreciation = function(investment, macro_projections, tax_law) {
  
  #----------------------------------------------------------------------------
  # Calculates depreciation deductions for all projected investment for all
  # years. Iterates over year to deal with RAM limitations.
  #
  # Parameters:
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
  
  # Calculate Adjustment Factors
  index_adjustment = indexes %>% 
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

  tax_law$schedules %>%
    left_join(
      investment %>%
        left_join(tax_law$params, by = c('form', 'year', 'asset_class', 'industry')),
      by = c('B', 'L', 'bonus', 's179'), relationship = 'many-to-many'
    ) %>%
    mutate(
      deduction_year = year + t
    ) %>%
    left_join(
      index_adjustment, by = c('deduction_year', 'indexation')
    ) %>%
    mutate(
      deduction = investment * schedule * factor
    ) %>%
    select(
      form, year, asset_class, industry, investment, deduction_year, deduction
    ) %>%
    pivot_wider(names_from = deduction_year, values_from = deduction) %>%
    mutate(across(.cols = everything(), .fns = ~ replace_na(., 0))) %>% 
    return()
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

  tibble(B, L, bonus, s179, schedule) %>%
    mutate(t = 1:length(L)) %>%
    return()

}

calc_macrs = function(B, L) {
  
  #----------------------------------------------------------------------------
  # Calculates MACRS schedule given decay rate and userful life params.
  #
  # Parameters:
  #  - B (dbl) : decay rate factor
  #  - L (dbl) : cost recovery life
  # 
  # Returns:
  #  - vector of deductions (dbl[])
  #----------------------------------------------------------------------------
  
  # Binary flag to switch from declining balance method to straight line method
  switch = F
  balance = 1
  deductions = c()
  
  # Loop over periods
  for (t in 1:ceiling(L)) {
    
    # Get straight line balance
    s_balance = straight_line(balance, L)
    
    # If straight line still hasn't been triggered... 
    if (!switch) {
      
      # Calculate and append declining balance deduction
      d_balance  = declining_balance(balance, B, L)
      balance    = d_balance[1]
      deductions = c(deductions, d_balance[2])
      
      # Check if straight line deduction has been triggered (i.e. is larger)
      if (s_balance[2] > d_balance[2]) {
        switch = T
        num    = sum(deductions)
        denom  = L - t
      } 
      
    # ...otherwise, apply straight line method
    } else {
      deductions = c(deductions, (1 - num) / denom)
    }
  }

  return(deductions)
}


calc_B_L = function(tax_law) {
  combinations = unique(tax_law$params[,c("B", "L")])
  
  1:nrow(combinations) %>%
    map(.f = ~ calc_macrs(combinations[.x,])) %>%
    bind_rows() %>%
    pivot_wider(names_from = col, names_glue = "t_{col}", values_from = deductions) %>%
    replace(is.na(.), 0) %>%
    return()
}

declining_balance = function(balance, B, L) {
  
  #----------------------------------------------------------------------------
  # Helper function to calculate declining next-period deduction under 
  # declining balance convention.
  #
  # Parameters:
  #  - balance (dbl) : existing balance
  #  - B       (dbl) : decay rate factor
  #  - L       (dbl) : cost recovery life
  # 
  # Returns:
  #  - tuple of new balance, next-period deduction (dbl[])
  #----------------------------------------------------------------------------
  
  d_balance = balance * (1 - B / L)
  return(c(d_balance, balance - d_balance))
}



straight_line = function(balance, L) {
  
  #----------------------------------------------------------------------------
  # Helper function to calculate declining next-period deduction under 
  # straight line convention.
  #
  # Parameters:
  #  - balance (dbl) : existing balance
  #  - L       (dbl) : cost recovery life
  # 
  # Returns:
  #  - tuple of new balance, next-period deduction (dbl[])
  #----------------------------------------------------------------------------

  s_balance = balance - 1 / L
  return(c(s_balance, balance - s_balance))
}


