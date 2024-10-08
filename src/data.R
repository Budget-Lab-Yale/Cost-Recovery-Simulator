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


  # Read and bind all four files  
  tax_law = c('L', 'B', 'bonus', 's179') %>% 
    map(
      .f = ~ file.path('./config/tax_law/', scenario_info$tax_law, paste0(.x, '.csv')) %>% 
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
    pivot_wider(names_from = param) %>%  
  
    # Join other (non asset-class-specific) parameters  
    left_join(
      file.path('./config/tax_law/', scenario_info$tax_law, 'other.csv') %>% 
        read_csv(show_col_types = F), 
      by = 'year'
    )
    
  # Extend series beyond last specified tax law year (assume constant policy)
  if (max(scenario_info$years) > max(tax_law$year)) { 
    tax_law %<>% 
      bind_rows(
        tax_law %>% 
          filter(year == max(year)) %>% 
          select(-year) %>% 
          expand_grid(year = (max(tax_law$year) + 1):max(scenario_info$years)) 
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
  
  # Build tax law
  tax_law = build_tax_law(scenario_info)
  
  # Read investment projections
  investment = c('historical.csv', 'projections.csv') %>% 
    map( 
      ~ file.path(scenario_info$paths$`Investment-Projections`, .x) %>% 
          read_csv(show_col_types = F)
    ) %>% 
    bind_rows() %>%
    pivot_longer(
      cols      = -year, 
      names_to  = 'asset_class', 
      values_to = 'investment'
    ) %>% 
  
    # Join tax law
    left_join(tax_law, by = c('year', 'asset_class')) %>% 
    
    # Filter to specified years
    filter(year %in% scenario_info$years) %>% 
    
    # Filter out variables with no corresponding tax law info 
    # (i.e. aggregated asset class summary variables in projections)
    filter(asset_class %in% unique(tax_law$asset_class)) %>%
    return()
}

