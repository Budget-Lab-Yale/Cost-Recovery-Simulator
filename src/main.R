library(tidyverse)
library(data.table)
library(magrittr)
library(yaml)
library(rlang)

# Create output root
time_stamp  = file.path(format(Sys.time(), '%Y%m%d%H'))
dir.create(file.path(getwd(), "resources/output", time_stamp))

# User-supplied parameters 
runscript_id = 'tests/expensing2023'

# Source all functions and define global variables
list.files('./src', recursive = T) %>% 
  walk(.f = ~ if (.x != 'main.R') source(file.path('./src/', .x)))

# Run simulation for all scenarios
walk(runscript$id, do_scenario, .progress = T) 


# Across-scenario post-processing comparisons

#Preliminaries
options(scipen=10000)
source("/gpfs/gibbs/project/sarin/ds3228/ylb_vis/programs/ybl_functions_mother.R")
baseline_path <- paste0("/resources/output/",format(Sys.time(), '%Y%m%d%H'),"/baseline/")
expenses_path <- paste0("/resources/output/",format(Sys.time(), '%Y%m%d%H'),"/expensing2023/")


dir.create(file.path(getwd(), "resources/output", time_stamp, "chart_packs"))

all_csvs <- as.data.frame(list.files(paste0(getwd(), "/resources/output/", time_stamp, "/baseline/totals")))
all_csvs <- all_csvs %>%
  gsub(pattern = '.csv', x =all_csvs[,1], replacement = "") %>%
  as.data.frame()
colnames(all_csvs) <- "csv_names"


colnames_data_list <- list()
for(name in 1:nrow(all_csvs)){
  data <-  read.csv(paste0(getwd(),
                           baseline_path,
                           paste0("totals/", all_csvs$csv_names[name], ".csv")))
  indx <- grepl('year|form|asset|class|industry', colnames(data))
  colnames_data <- list(colnames(data[indx]))
  colnames_data_list <- append(map_chr(colnames_data, str_c, collapse = ","), colnames_data_list)
}
all_csvs$cols_to_group_by <- unlist(rev(colnames_data_list))

#Graph
for(soto in 1:nrow(all_csvs)){
  graphing(csv_name = all_csvs$csv_names[soto], cols_to_group_by = strsplit(all_csvs$cols_to_group_by[soto], ",")[[1]])
}



