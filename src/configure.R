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





build_tax_law = function(id) {
  
  #----------------------------------------------------------------------------
  # Creates tax law dataframe
  #
  # Parameters:
  #  - id (str) : scenario ID
  # 
  # Returns:
  # - tax law dataframe (df)
  #----------------------------------------------------------------------------
  
  # Read and bind all four files  
  c('L', 'B', 'bonus', 's179') %>% 
    map(
      .f = ~ file.path('./config/tax_law/baseline/', paste0(.x, '.csv')) %>% 
        read_csv(show_col_types = F) %>% 
        mutate(param = .x) 
    ) %>% 
    bind_rows() %>% 
    
    # Reshape long in year
    pivot_longer(
      cols            = -c(asset_class, param), 
      names_to        = 'year', 
      names_transform = as.integer
    ) %>% 
    select(year, asset_class, param, value) %>% 
    arrange(year, asset_class) %>% 
    return()
}
