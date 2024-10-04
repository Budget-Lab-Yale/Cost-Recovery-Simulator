#----------------------------------------------------------
# configure.R
# 
# Functions to configure interface paths and parse tax law
#----------------------------------------------------------


configure_globals = function(runscript_id) {
  
  #----------------------------------------------------------------------------
  # TODO build out 
  # 
  # 
  #----------------------------------------------------------------------------
 
  
  raw_config = read_yaml('./config/baseline.yaml')
  
  
  interface_paths = raw_config$dependency_info %>%
    map2(.y = names(.),
         .f = ~ file.path(
           '/gpfs/gibbs/project/sarin/shared/',
           .x$type,
           .y,
           paste0('v', .x$version),
           .x$vintage,
           .x$scenario)
    )
  
  st      = Sys.time()
  vintage = paste0(lubridate::year(st),
                   lubridate::month(st) %>%
                     paste0('0', .) %>%
                     str_sub(-2),
                   lubridate::day(st) %>%
                     paste0('0', .) %>%
                     str_sub(-2),
                   lubridate::hour(st) %>%
                     paste0('0', .) %>%
                     str_sub(-2))
  
  return(
    list(
      interface_paths = interface_paths
    )
  )
  
}





build_tax_law = function(id, last_year) {
  
  #----------------------------------------------------------------------------
  # Creates tax law dataframe based on four input files. Extends series 
  # beyond last year if specified.
  #
  # Parameters:
  #  - id        (str) : scenario ID
  #  - last_year (int) : last year
  # 
  # Returns:
  # - tax law dataframe (df)
  #----------------------------------------------------------------------------
  
  # Read and bind all four files  
  tax_law = c('L', 'B', 'bonus', 's179') %>% 
    map(
      .f = ~ file.path('./config/tax_law/baseline/', paste0(.x, '.csv')) %>% 
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
  
  # Extend series 
  if (last_year > max(tax_law$year)) { 
    tax_law %<>% 
      bind_rows(
        tax_law %>% 
          filter(year == max(year)) %>% 
          select(-year) %>% 
          expand_grid(year = max(tax_law$year):last_year) 
      )
  }
  
  tax_law %>% 
    arrange(year, asset_class) %>% 
    return()
}


