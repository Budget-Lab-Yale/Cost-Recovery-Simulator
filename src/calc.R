calc_depreciation = function(expenses, year, years, revenue = T) {
  df = expenses %>%
    pivot_longer(!year, names_to = 'schedule', values_to = 'investment') %>%
    left_join(., rate_schedule) %>%
    mutate(across(starts_with('V'),
                  ~ .x * investment)
    )
  
  if(revenue) {
    df %>%
      rename_with(~as.character(year - 1 + as.integer(str_sub(., 2))), .cols = starts_with('V')) %>%
      fill_years(., years) %>%
      pivot_longer(!c(year, schedule, investment), names_to = 'd_year', values_to = 'depreciation') %>%
      arrange(d_year) %>%
      filter(d_year %in% as.integer(years)) %>%
      pivot_wider(names_from = d_year, values_from = depreciation) %>%
      return()
    
  } else {
    df %>%
      rename_with(~ paste0('t+', as.integer(str_sub(., 2)) - 1), .cols = starts_with('V')) %>%
      return()
  }
  
}

fill_years = function(df, cols) {
  add = cols[!cols %in% names(df)]
  if(length(add) != 0) df[add] = 0
  return(df)
}