#----------------------------------------------------------
# calc.R
# 
# Contains functions to calculate depreciation deductions
# given investment projections and tax law parameters
#----------------------------------------------------------


calc_all_depreciation = function(investment, macro_projections) {
  
  # Get all possible years a deduction can be taken
  all_years = min(investment$year):(max(investment$year) + max(investment$L))

  # Parse indexation values
  indexes = macro_projections %>% 
    mutate(inflation = cpiu / lag(cpiu) - 1, timevalue = tsy_10y / 100) %>% 
    select(year, inflation, timevalue)
  
  # Iterate over each asset class-year observation
  1:nrow(investment) %>%
    
    # Calculate depreciation deductions
    map(.f = ~ calc_depreciation(investment[.x,], indexes, all_years)) %>%
    bind_rows() %>%
    
    # Reshape wide in deduction year
    pivot_wider(names_from = deduction_year) %>% 
    
    # Replace NAs (no deduction that year) with 0s
    mutate(across(.cols = -year, .fns = ~ replace_na(., 0))) %>% 
    return()
}



calc_depreciation = function(investment, indexes, all_years) {
  
  # Extract values from tibble
  year    = investment$year       # Year of investment
  L       = investment$L          # Cost recovery period
  B       = investment$B          # Decay rate
  bonus   = investment$bonus      # Section 168(k) "bonus" depreciation rate
  s179    = investment$s179       # Share of investment eligible for section 179 expensing
  balance = investment$investment # Initial investment basis
  
  # Calculate balance to be depreciated after accounting for amount expensed
  expensed = balance * (s179 + bonus * (1 - s179))
  balance  = balance - expensed
  
  # Calculate schedule of MACRS deductions
  deductions = calc_macrs(balance, B, L)
  
  # Add expensed amount to schedule 
  deductions[1] = deductions[1] + expensed
  
  # Account for half-year convention (assume all investment occurs in middle of the year)
  out = (deductions / 2) + lag(deductions / 2, default = 0)
  
  # Adjust for inflation/time value if specified under law
  if (investment$indexation %in% c('inflation', 'timevalue')) {
    
    # Calculate and apply path of indexation adjustment factors
    index_years = year:(year + length(out) - 1)
    index_adjustment = indexes %>% 
      filter(year %in% index_years) %>% 
      select(value = !!investment$indexation) %>% 
      mutate(value = cumprod(1 + lag(value, default = 0))) %>% 
      deframe()
    out = out * index_adjustment
    
  } else {
    if (investment$indexation != 'none') {
      stop('Invalid tax law parameter value for indexation')
    }
  }
  
  # Construct output row and return
  L = ceiling(L)
  tibble(deduction_year = year:(year + L), value = c(out, deductions[L]/2)) %>%
    mutate(
      form        = investment$form,
      year        = year, 
      asset_class = investment$asset_class, 
      industry    = investment$industry,
      investment  = investment$investment
    ) %>% 
    select(form, year, asset_class, industry, investment, deduction_year, value) %>% 
    return()
}



calc_macrs = function(balance, B, L) {
  # Binary flag to switch from Declining Balance to Straight Line deduction
  switch = F
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

