calc_depreciation = function(expenses) {
  
  # Pulls number from schedule TODO
  h = max(as.numeric(gsub("([0-9]+).*$", "\\1", expenses$schedule)))
  
  # All possible years a deduction can be taken
  all_years = c(unique(expenses$year), (max(expenses$year)+1):(max(expenses$year) + h))
  
  1:nrow(expenses) %>%
    map(.f = ~ calc_macrs(expenses[.x,], all_years)) %>%
    bind_rows() %>%
    return()
  
}

calc_macrs = function(expenses, all_years) {
  
  # Recasts values to be more usuable
  
  # Year of investment
  year = as.integer(expenses["year"])
  # Depreciation schedule TODO
  schedule = expenses["schedule"]
  # Length of schedule
  L = as.numeric(gsub("([0-9]+).*$", "\\1", schedule))
  # Asset class parameter
  b = as.numeric(expenses["b"])
  
  # Calculate balance to be expensed, after withrdawing bonus
  balance = as.numeric(expenses["investment"])
  bonus = balance * as.numeric(expenses["bonus"])
  balance = balance - bonus
  
  deductions = apply_deduction(balance, b, L)
  
  # Add in bonus and calculate for half year splits
  deductions[1] = deductions[1] + bonus
  out = deductions[1] / 2
  
  for(i in 2:length(deductions)) {
    out = c(out, deductions[i-1]/2 + deductions[i]/2)
  }
  
  # Set up and Construct output row
  schedule = rep(schedule, L+1)
  years = year:(year+L)
  year = rep(year, L+1)
  out = c(out, deductions[L]/2)
  
  tibble(year, schedule, years, out) %>%
    pivot_wider(names_from = years, values_from = out) %>%
    # Add columns with zeroes for all years in which a deduction isn't taken
    fill_years(., all_years) %>%
    pivot_longer(!c(year, schedule), names_to = 'years', values_to = 'depreciation') %>%
    arrange(years) %>%
    pivot_wider(names_from = years, values_from = depreciation) %>%
    return()
}

apply_deduction = function(balance, b, L) {
  # Binary flag to switch from Declining Balance to Straight Line deduction
  switch = F
  deductions = c()
  
  for(i in 1:L) {
    s_balance = straight_line(balance, L)
    
    if(!switch) {
      # Calculate and append Declining Balance deduction
      d_balance = declining_balance(balance, b, L)
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

declining_balance = function(balance, b, L) {
  d_balance = balance * (1 - b/L)
  return(c(d_balance, balance - d_balance))
}

straight_line = function(balance, L) {
  s_balance = balance - 1/L
  return(c(s_balance, balance - s_balance))
}


fill_years = function(df, cols) {
  add = as.character(cols[!cols %in% names(df)])
  if(length(add) != 0) df[add] = 0
  return(df)
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




# calc_depreciation = function(expenses, year, years, revenue = T, tsy = NULL) {
#   df = expenses %>%
#     pivot_longer(!year, names_to = 'schedule', values_to = 'investment') %>%
#     left_join(., rate_schedule) %>%
#     mutate(
#       bonus = bonus * investment,
#       investment = investment - bonus,
#       across(starts_with('V'),
#              ~ .x * investment),
#       V1 = V1 + bonus
#     ) %>%
#     select(!bonus)
#   
#   if(revenue) {
#     df %>%
#       rename_with(~as.character(year - 1 + as.integer(str_sub(., 2))), .cols = starts_with('V')) %>%
#       fill_years(., years) %>%
#       pivot_longer(!c(year, schedule, investment), names_to = 'd_year', values_to = 'depreciation') %>%
#       arrange(d_year) %>%
#       filter(d_year %in% as.integer(years)) %>%
#       pivot_wider(names_from = d_year, values_from = depreciation) %>%
#       return()
#     
#   } else {
#     df %>%
#       rename_with(~ paste0('t+', as.integer(str_sub(., 2)) - 1), .cols = starts_with('V')) %>%
#       return()
#   }
#   
# }


# npv = depreciation_npv %>% filter(year == 2025) %>%
#   pivot_longer(!c(year, schedule, investment), names_to = 'period', values_to = 'depreciation') %>%
#   left_join(., tsy) %>%
#   mutate(
#     npv = depreciation / (1 + tsy_10y + spread)^(as.integer(str_sub(period, -1)))
#   ) %>%
#   select(year, schedule, period, npv) %>%
#   pivot_wider(names_from = period, values_from = npv) %>%
#   mutate(
#     npv = rowSums(across(starts_with('t+')))
#   ) %>%
#   select(!contains('t+'))
# 
# 
# 
# tsy = 'projections.csv' %>%
#   map(.f = ~ file.path(interface_paths$`Macro-Projections`, .x) %>%
#         read_csv()
#   ) %>%
#   bind_rows() %>%
#   filter(between(year, 2025, 2025 + 20)) %>%
#   select(year, tsy_10y) %>%
#   mutate(
#     tsy_10y = (tsy_10y / 100) + 1, 
#     spread = .02,
#     period = paste0('t+', year - 2025)
#   ) %>%
#   select(!year)



