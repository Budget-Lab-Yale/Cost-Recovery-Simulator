#----------------------------------------------------------
# data.R
# 
# Contains functions to build tax law and investment data, 
# which is input into calculation functions
#----------------------------------------------------------


build_tax_law = function(scenario_info) {
  
  #----------------------------------------------------------------------------
  # Creates tax law parameters dataframe. Extends series beyond last given 
  # year if specified in runscripts.
  #
  # Parameters:
  #  - scenario_info (list) : scenario info object (see get_scenario_info())
  # 
  # Returns:
  # - tax law parameters dataframe (df)
  #----------------------------------------------------------------------------

  # Read core tax law file
  tax_law = file.path('./config/tax_law/', paste0(scenario_info$tax_law, '.csv')) %>% 
    read_csv(show_col_types = F)

  # For each multi-dimension parameter...
  params = list()
  for (param in c('B', 'L', 'bonus', 's179')) {
    
    # Get unique set of filenames for parameter settings 
    settings = tax_law[param] %>% distinct() %>% deframe()
    
    # Read all parameter setting files...
    params[[length(params) + 1]] = settings %>% 
      map(
        .f = ~ file.path('./config/tax_law/params/', param, paste0(.x, '.csv')) %>% 
          read_csv(show_col_types = F) %>% 
          mutate(param = param, param_setting = as.character(.x), .before = everything())  
      ) %>% 
      
      # Reshape long in asset class
      bind_rows() %>% 
      pivot_longer(
        cols      = -c(industry, param, param_setting), 
        names_to  = 'asset_class', 
        values_to = 'value'
      )
  }
  
  # Bind into single dataframe
  params = bind_rows(params)
  
  # Set up structure: asset X industry X year X legal form X param X setting 
  tax_law = params %>% 
    distinct(asset_class, industry) %>% 
    expand_grid(tax_law) %>% 
    pivot_longer(
      cols             = c(B, L, bonus, s179), 
      names_to         = 'param', 
      values_to        = 'param_setting',
      values_transform = as.character
    ) %>% 
    
    # Join parameter setting values 
    left_join(params, by = c('param', 'param_setting', 'asset_class', 'industry')) %>% 
    select(-param_setting) %>% 
    pivot_wider(names_from = param)

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
    select(form, year, everything()) %>% 
    arrange(form, year, asset_class, industry) %>% 
    return()
}



build_schedules = function(params) {
  
  #----------------------------------------------------------------------------
  # Builds cost recovery schedules for all combinations of B, L, bonus, and 
  # s179.
  #
  # Parameters:
  #  - scenario_info (list) : scenario info object (see get_scenario_info())
  # 
  # Returns:
  #  - dataframe of schedules, long in relative time from investment year (df)
  #----------------------------------------------------------------------------
  
  # Get all combos
  params %>% 
    distinct(B, L, bonus, s179) %>% 
    as.list() %>% 
    pmap(calc_schedule, max_t = max(params$L) + 1) %>% 
    bind_rows() %>% 
    return()
}



build_investment_data = function(scenario_info) {
  
  #----------------------------------------------------------------------------
  # Reads investment data (historical and projections) and puts it into long 
  # format. 
  #
  # Parameters:
  #  - scenario_info (list) : scenario info object (see get_scenario_info())
  # 
  # Returns:
  # - investment data (df)
  #----------------------------------------------------------------------------
  
  # Read investment projections
  c('historical.csv', 'projections.csv') %>% 
    map( 
      ~ file.path(scenario_info$paths$`Investment-Projections`, .x) %>% 
          read_csv(show_col_types = F)
    ) %>% 
    bind_rows() %>%
    pivot_longer(
      cols      = -year, 
      names_to  = c('asset_class', 'industry'),
      names_sep = '[.]',
      values_to = 'investment'
    ) %>% 

    # Filter to specified years
    filter(year %in% scenario_info$years) %>% 
    
    # Impute investment by legal form
    left_join(
      build_ccorp_shares(), 
      by = c('industry', 'asset_class')
    ) %>% 
    mutate(
      ccorp = investment * ccorp_share, 
      pt    = investment * (1 - ccorp_share)
    ) %>% 
    select(-standard_industry, -ccorp_share, -investment) %>% 
    pivot_longer(
      cols      = c(pt, ccorp), 
      names_to  = 'form', 
      values_to = 'investment'
    ) %>% 
    select(year, form, everything()) %>% 
    arrange(year, form) %>% 
    return()
}



build_macro_projections = function(scenario_info) { 
  
  #----------------------------------------------------------------------------
  # Reads macro projections data (historical and projections). 
  #
  # Parameters:
  #  - scenario_info (list) : scenario info object (see get_scenario_info())
  # 
  # Returns:
  # - macro projections data (df)
  #----------------------------------------------------------------------------
  
  c('historical.csv', 'projections.csv') %>% 
    map(.f = ~ read_csv(file.path(scenario_info$paths$`Macro-Projections`, .x), show_col_types = F)) %>% 
    bind_rows() %>% 
    return()
} 


build_ccorp_shares = function() {
  
  #----------------------------------------------------------------------------
  # TODO
  #
  # Parameters:
  #  - TODO
  # 
  # Returns:
  #  - TODO
  #----------------------------------------------------------------------------
  
  # Read C corp shares by asset class and industry 
  read_csv('./resources/industry_crosswalk.csv', show_col_types = F) %>% 
    left_join(
      read_csv('./resources/ccorp_share.csv', show_col_types = F) %>% 
        pivot_longer(
          cols      = -standard_industry, 
          names_to  = 'asset_class', 
          values_to = 'ccorp_share'
        ), 
      by = 'standard_industry', 
      relationship = 'many-to-many'
    ) 
  
  # TODO adjust for compositional shifts over time
  
}
