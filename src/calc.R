#----------------------------------------------------------
# calc.R
# 
# Contains functions to calculate depreciation deductions
# given investment projections and tax law parameters
#----------------------------------------------------------

calc_all_depreciation = function(investment, macro_projections) {
  
  # Pulls number from schedule TODO
  h = max(investment$L)
  
  # All possible years a deduction can be taken
  all_years = c(unique(investment$year), (max(investment$year)+1):(max(investment$year) + h))

  # Parse indexation values
  indexes = macro_projections %>% 
    mutate(inflation = cpi / lag(cpi) - 1) %>% 
    select(year, inflation, timevalue = yield)
  
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
  
  # Recasts values to be more usuable
  
  # Year of investment
  year = as.integer(investment["year"])
  # Length of schedule
  L = as.numeric(investment["L"])
  # Asset class parameter
  B = as.numeric(investment["B"])
  # Section 179 parameter
  s179 = as.numeric(investment["s179"])
  
  # Calculate balance to be depreciated after accounting for amount expensed
  balance = as.numeric(investment["investment"])
  expensed = balance * (s179 + as.numeric(investment["bonus"]) * (1 - s179))
  balance = balance - expensed
  
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
  
  # Construct output row
  L = ceiling(L)
  
  tibble(
    deduction_year = year:(year + L), 
    value = c(out, deductions[L]/2)
  ) %>%
    mutate(
      year = year, 
      asset_class = investment$asset_class
    ) %>% 
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



calc_npv = function(df) {
  tsy = 'projections.csv' %>%
    map(.f = ~ file.path(interface_paths$`Macro-Projections`, .x) %>%
          read_csv()
    ) %>%
    bind_rows() %>%
    select(year, tsy_10y) %>%
    mutate(
      tsy_10y = (tsy_10y / 100) + 1, 
      spread = .02
    ) 
  
  df %>% left_join(., df$year %>%
    map(.f = ~ df %>% filter(year == .x) %>%
          pivot_longer(!c(year, schedule, investment), names_to = 'depreciation') %>%
          left_join(.,
            tsy %>% filter(between(year, .x, .x + 20)) %>%
              mutate(period = paste0('t+', year - .x)) %>%
              select(!year), by = join_by(year, schedule)
          ) %>%
          mutate(
            npv = depreciation / (1 + tsy_10y + spread)^(as.integer(str_sub(period, -1)))
          ) %>%
          select(year, schedule, period, npv) %>%
          pivot_wider(names_from = period, values_from = npv) %>%
          mutate(
            npv = rowSums(across(starts_with('t+')))
          ) %>%
          select(!starts_with('t+'))
        ) %>%
      bind_rows(), by = join_by(year, schedule)
    ) %>%
    return()
  
}


