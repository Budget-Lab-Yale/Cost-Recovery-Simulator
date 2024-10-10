#----------------------------------------------------------
# calc.R
# 
# Contains functions to calculate depreciation deductions
# given investment projections and tax law parameters
#----------------------------------------------------------


calc_all_depreciation = function(investment, macro_projections) {
  
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
    investment %>% 
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
      mutate(deduction = deduction * factor)
          
      # Reshape wide in deduction year(saves memory)
      select(year, form, asset_class, industry, investment, deduction_year, deduction) %>% 
      pivot_wider(
        names_from  = deduction_year, 
        values_from = deduction
      )
  }
   
  # Bind years together and replace NAs (no deduction that year) with 0s
  output %>% 
    bind_rows() %>%
    mutate(across(.cols = everything(), .fns = ~ replace_na(., 0))) %>% 
    return()
}



calc_schedule = function(max_t, B, L, bonus, s179) {
  
  # Calculate amount avilable for immediate expensing
  expensed = s179 + bonus * (1 - s179)
  
  # Calculate remaining MACRS schedule and add in expensed amount
  schedule    = (1 - expensed) * calc_macrs(B, L)
  schedule[1] = schedule[1] + expensed

  # Account for half-year convention (assume all investment occurs in middle of the year)
  schedule = c(schedule / 2, 0) + c(0, schedule / 2)

  # Return output in df format
  tibble(
    t     = 1:ceiling(max_t), 
    share = c(schedule, rep(0, ceiling(max_t) - length(schedule)))
  ) %>% 
  mutate(B = B, L = L, bonus = bonus, s179 = s179, .before = everything()) %>% 
  return()

}



calc_macrs = function(B, L) {
  
  # Binary flag to switch from Declining Balance to Straight Line deduction
  switch = F
  balance = 1
  deductions = c()
  
  for(i in 1:ceiling(L)) {
    s_balance = straight_line(balance, L)
    
    if(!switch) {
      # Calculate and append Declining Balance deduction
      d_balance = declining_balance(balance, B, L)
      balance = d_balance[1]
      deductions = c(deductions, d_balance[2])
      
      # Check if Straight Line deduction becomes preferred
      if(s_balance[2] > d_balance[2]) {
        switch = T
        num = sum(deductions)
        denom = L - i
      } 
    } else {
      # Apply simplified straightline once more valuable
      deductions = c(deductions, (1 - num)/denom)
    }
  }
  
  return(deductions)
}



declining_balance = function(balance, B, L) {
  d_balance = balance * (1 - B/L)
  return(c(d_balance, balance - d_balance))
}



straight_line = function(balance, L) {
  s_balance = balance - 1/L
  return(c(s_balance, balance - s_balance))
}


