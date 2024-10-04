#----------------------------------------------------------
# data.R
# 
# Contains functions to build tax law and investment data, 
# which is input into calculation functions
#----------------------------------------------------------


build_tax_law = function(scenario_info) {
  
  #----------------------------------------------------------------------------
  # Creates tax law dataframe based on four input files. Extends series 
  # beyond last given year if specified in runscripts.
  #
  # Parameters:
  #  - scenario_info (list) : scenario info object (see get_scenario_info())
  # 
  # Returns:
  # - tax law dataframe (df)
  #----------------------------------------------------------------------------
  
  # Extract elements of scenario info
  id    = scenario_info$id
  years = scenario_info$years
  
  # Read and bind all four files  
  tax_law = c('L', 'B', 'bonus', 's179') %>% 
    map(
      .f = ~ file.path('./config/tax_law/', id, paste0(.x, '.csv')) %>% 
        read_csv(show_col_types = F) %>% 
        mutate(param = .x) 
    ) %>% 
    bind_rows() %>% 
    
    # Reshape long in year and wider in parameter
    pivot_longer(
      cols            = -c(asset_class, param), 
      names_to        = 'year', 
      names_transform = as.integer
    ) %>% 
    select(year, asset_class, param, value) %>% 
    pivot_wider(names_from = param) 
  
  # Extend series beyond last specified tax law year (assume constant policy)
  if (max(years) > max(tax_law$year)) { 
    tax_law %<>% 
      bind_rows(
        tax_law %>% 
          filter(year == max(year)) %>% 
          select(-year) %>% 
          expand_grid(year = max(tax_law$year):max(years)) 
      )
  }
  
  tax_law %>% 
    arrange(year, asset_class) %>% 
    return()
}



build_investment_data = function(scenario_info) {
  
  #----------------------------------------------------------------------------
  # Reads investment data and joins tax law.
  #
  # Parameters:
  #  - scenario_info (list) : scenario info object (see get_scenario_info())
  # 
  # Returns:
  # - investment + tax law dataframe (df)
  #----------------------------------------------------------------------------
  
  
  # Extract elements of scenario info
  id    = scenario_info$id
  years = scenario_info$years
  
  # Read investment projections
  investment = c('historical_data.csv', 'baseline_projections.csv') %>% 
    map(~ read_csv(file.path('./resources/input/', id, .x), show_col_types = F)) %>% 
    bind_rows() %>%
    pivot_longer(
      cols      = -year, 
      names_to  = 'asset_class', 
      values_to = 'investment'
    ) %>% 
  
    # Build and join tax law
    left_join(build_tax_law(scenario_info), by = c('year', 'asset_class')) %>% 
    
    # Filter to specified years
    filter(year %in% years) %>% 
    return()
}

