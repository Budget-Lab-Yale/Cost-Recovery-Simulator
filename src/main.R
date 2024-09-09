library(yaml)
library(tidyverse)
library(data.table)

start = Sys.time()

list.files('./src', recursive = T) %>% 
  walk(.f = ~ if (.x != 'main.R') source(file.path('./src/', .x)))

rate_schedule = build_full_schedule('./config/schedules/baseline/rates.yaml')

expenses = read_csv('/gpfs/gibbs/project/sarin/shared/raw_data/Depreciation/test/expenses.csv')

years = expenses$year %>% as.character()

depreciation_rev = expenses$year %>%
  map(.f = ~ calc_depreciation(filter(expenses, year==.x), .x, as.character(expenses$year))
      ) %>%
  bind_rows() %>%
  mutate(year = as.character(year)) %>%
  bind_rows(
      reframe(
        ., 
        across(where(is.numeric), sum),
        across(where(is.character), ~ "Total")
      )
  )

depreciation_npv = expenses$year %>%
  map(.f = ~ calc_depreciation(filter(expenses, year==.x), .x, as.character(expenses$year), revenue = F)
  ) %>%
  bind_rows() %>%
  mutate(year = as.character(year)) %>%
  bind_rows(
    reframe(
      ., 
      across(where(is.numeric), sum),
      across(where(is.character), ~ "Total")
    )
  )

end = Sys.time()

as.numeric(end - start)