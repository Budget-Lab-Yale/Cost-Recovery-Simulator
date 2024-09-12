library(yaml)
library(tidyverse)
library(data.table)
library(magrittr)

start = Sys.time()

# Initialize Functions, data, and Tax Law
list.files('./src', recursive = T) %>% 
  walk(.f = ~ if (.x != 'main.R') source(file.path('./src/', .x)))

expenses = read_csv('/gpfs/gibbs/project/sarin/shared/raw_data/Depreciation/test/expenses.csv') %>%
  pivot_longer(!year, names_to = 'schedule', values_to = 'investment')


tax_law = build_depreciation('./config/schedules/baseline', as.character(expenses$year))

# Attach Tax Law (not working)
expenses %<>%
  left_join(., tax_law)


# Asset class with schedule
depreciation_rev = calc_depreciation(expenses)

end = Sys.time()

as.numeric(end - start)

# DO DEDUCTIONS BY DEDUCTION YEAR








# SCRATCH
# 
# depreciation_rev = expenses$year %>%
#   map(.f = ~ calc_depreciation(filter(expenses, year==.x), .x, as.character(expenses$year))
#       ) %>%
#   bind_rows() %>%
#   mutate(year = as.character(year)) %>%
#   bind_rows(
#       reframe(
#         ., 
#         across(where(is.numeric), sum),
#         across(where(is.character), ~ "Total")
#       )
#   )
# 
# depreciation_npv = expenses$year %>%
#   map(.f = ~ calc_depreciation(filter(expenses, year==.x), .x, as.character(expenses$year), revenue = F)
#   ) %>%
#   bind_rows() %>%
#   calc_npv() %>%
#   mutate(year = as.character(year)) %>%
#   bind_rows(
#     reframe(
#       ., 
#       across(where(is.numeric), sum),
#       across(where(is.character), ~ "Total")
#     )
#   )
# 
# frog = depreciation_npv %>%
#   filter(year != 'Total') %>%
#   mutate(year = as.integer(year)) %>%
#   calc_npv()
#     
# 
# 
# 
# 
# npv = depreciation_npv %>% filter(year == 'Total')
