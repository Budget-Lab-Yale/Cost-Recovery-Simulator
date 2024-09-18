library(yaml)
library(tidyverse)
library(data.table)
library(magrittr)

start = Sys.time()

# Initialize Functions, data, and Tax Law
list.files('./src', recursive = T) %>% 
  walk(.f = ~ if (.x != 'main.R') source(file.path('./src/', .x)))

# switch this for the macro path once Maddie is done

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

Deductions_by_Year = deductions_by_year(depreciation_rev)
