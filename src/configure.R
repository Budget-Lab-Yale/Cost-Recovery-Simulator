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

build_depreciation = function(root_path, years) {
  
  years = as.integer(years)
  
  print(root_path)
  
  build_full_schedule(file.path(root_path, 'b.yaml'), unique(expenses$year))%>%
    pivot_longer(!year, names_to = 'schedule', values_to = 'b') %>%
    left_join(., build_full_schedule(file.path(root_path, 'bonus.yaml'), unique(expenses$year)) %>%
                pivot_longer(!year, names_to = 'schedule', values_to = 'bonus')) %>%
    return()
}

build_full_schedule = function(path, years) {
  
  raw_config = read_yaml(path)

  specified_years = raw_config %>%
    names() %>%
    as.integer()
  
  
  df = tibble(year = years) %>%
    left_join(tibble(year = specified_years,
                     value = raw_config),
              by='year') %>%
    fill(value)
  
  1:nrow(df) %>%
    map(.f = ~ build_one_schedule(df$value[.x][[1]], df$year[.x])) %>%
    bind_rows() %>% 
    select(!schedule) %>%
    return()
}

build_one_schedule = function(raw, year) {
  
  types = sapply(raw, length)
  year_max = seq_len(max(types))
  
  t(sapply(raw, '[', i = year_max)) %>%
    as.data.frame() %>%
    replace(is.na(.), 0) %>%
    rownames_to_column(., var='schedule') %>%
    mutate(year = year) %>%
    relocate(year, schedule) %>%
    return()
}


