library(tidyverse)
library(data.table)
library(magrittr)
library(yaml)

# Create output root
time_stamp  = file.path(format(Sys.time(), '%Y%m%d%H'))
output_root = file.path(platform_defaults$roots$output, time_stamp)
dir.create(file.path(getwd(), str_sub(output_root, 3, -1)), recursive = TRUE)

# User-supplied parameters 
runscript_id = 'tests/expensing2023'

# Source all functions and define global variables
list.files('./src', recursive = T) %>% 
  walk(.f = ~ if (.x != 'main.R') source(file.path('./src/', .x)))

# Run simulation for all scenarios
walk(runscript$id, do_scenario, .progress = T) 


# TODO across-scenario post-processing comparisons

#--------------------------------------------------------------------------#
#                               BASELINE                                   #
#--------------------------------------------------------------------------#
# Author Below these lines: Dylan Saez
# Please reach out if you have any questions
# Purpose: Calculate deltas for the output compared from baseline to projection
#     with input from the Depreciation model results above.

#Preliminaries - More file path
options(scipen=10000)
source("/gpfs/gibbs/project/sarin/ds3228/ylb_vis/programs/ybl_functions_mother.R")
baseline_path <- paste0("/resources/output/",format(Sys.time(), '%Y%m%d%H'),"/baseline/")
expenses_path <- paste0("/resources/output/",format(Sys.time(), '%Y%m%d%H'),"/expensing2023/")

#---Just want to take a gander at the data!
#baseline_data <- read.csv(paste0(getwd(), baseline_path, "detail/detail.csv"))
#rm(baseline_data)
#---- Okay, I'm done!

#exp2023_by_deduction_year <-  read.csv(paste0(getwd(), exp2023_path, "totals/by_deduction_year.csv"))
#Colnames
#   Form - ccorp, pt
#   Deduction_Year
#   Depreciation
#   NOL
#   Total
#--------------------------------------------
#Let's create a general function where I can give it any CSV
# Input: Any CSV
# Output: Deltas based on how I tell it to calculate

# delta_calc <- function(x) ((x - first(x)) / first(x)) * 1
# 
# delta_function <- function(csv_name, multiplier, group_by_names = c("form", "deduction_year")){
#   colnames_csv <- colnames(baseline_by_deduction_year)[!(colnames(baseline_by_deduction_year) %in% group_by_names)]
# 
#   delta_csv <- baseline_by_deduction_year %>%
#     group_by(subset(baseline_by_deduction_year, select = names(baseline_by_deduction_year) %in% group_by_names)) %>%
#     mutate(.cols = columnnames_csv, .fns(~. / first(.))....


#---AHHHHH will come back to this - I was confused as to why there were so many
#   zeros, I manually wrote it out below and realized the first year will of
#     course be zero!!! So now I know what is wrong. In any case, I will turn
#     these values into charts. Then circle back to finish off the above function
#     so we can get rid of these blocks below.
############################################################################
#                               BASELINE                                   #
############################################################################
#------------------------------------------------------------------------------
baseline_by_deduction_year <-  read.csv(paste0(getwd(), baseline_path,
                                               "totals/by_deduction_year.csv"))
baseline_by_deduction_year = baseline_by_deduction_year[-1,]

baseline_man_data <- baseline_by_deduction_year %>%
  group_by(form) %>%
  mutate(delta_depreciation = ((depreciation - first(depreciation)) * 1),
         delta_nol = ((nol - first(nol)) * 1),
         delta_total = ((total - first(total)) * 1)) %>%
  select(form, deduction_year, delta_depreciation, delta_nol, delta_total)

write.csv(baseline_man_data, paste0(getwd(), baseline_path,
                                    "deltas/by_deduction_year.csv"))

scenario <- baseline_man_data %>%
  pivot_longer(cols = -c(form ,deduction_year),
               names_to = "scenario",
               values_to = "value")

baseline_by_deduction_lc_ccorp <- scenario %>%
  filter(form == "ccorp") %>%
  ggplot() +
  geom_line(aes(x = deduction_year, y = value, color = scenario), linewidth = 1) +
  geom_hline(yintercept=0, linetype="solid", color = "black", size = .3) +
  #theme_bly_style() +
  labs(x = "Year",
       y = "Difference from Baseline",
       title = "Baseline by Deduction: Form == CC",
       subtitle = NULL,
       caption = NULL) +
  theme(#axis.title.x.bottom = element_blank(),
    #axis.title.y.left = element_blank(),
    axis.title.y.right = element_blank(),
    axis.text.x.bottom = element_text(size = 5.5),
    #axis.text.y.left = element_text(),
    plot.caption = element_markdown(size = 6),
    axis.text.y.right = element_blank(),
    #panel.grid.major.y = element_line(),
    #panel.grid.minor.y = element_line(),
    legend.position = "bottom")
baseline_by_deduction_lc_ccorp

baseline_by_deduction_lc_pt <- scenario %>%
  filter(form != "ccorp") %>%
  ggplot() +
  geom_line(aes(x = deduction_year, y = value, color = scenario), linewidth = 1) +
  geom_hline(yintercept=0, linetype="solid", color = "black", size = .3) +
  #theme_bly_style() +
  labs(x = "Year",
       y = "Difference from Baseline",
       title = "Baseline by Deduction: Form == PT",
       subtitle = NULL,
       caption = NULL) +
  theme(#axis.title.x.bottom = element_blank(),
    #axis.title.y.left = element_blank(),
    axis.title.y.right = element_blank(),
    axis.text.x.bottom = element_text(size = 5.5),
    #axis.text.y.left = element_text(),
    plot.caption = element_markdown(size = 6),
    axis.text.y.right = element_blank(),
    #panel.grid.major.y = element_line(),
    #panel.grid.minor.y = element_line(),
    legend.position = "bottom")
baseline_by_deduction_lc_pt
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
baseline_recovery_ratios <-  read.csv(paste0(getwd(), baseline_path,
                                               "totals/recovery_ratios.csv"))

baseline_man_data <- baseline_recovery_ratios %>%
  mutate(delta_real = ((real - first(real)) * 1),
         delta_pv = ((pv - first(pv)) * 1)) %>%
  select(year, delta_real, delta_pv)

write.csv(baseline_man_data, paste0(getwd(), baseline_path,
                                    "deltas/recovery_ratios.csv"))

scenario <- baseline_man_data %>%
  pivot_longer(cols = -c(year),
               names_to = "scenario",
               values_to = "value")

brr_lc <- ggplot(scenario) +
  geom_line(aes(x = year, y = value, color = scenario), linewidth = 1) +
  geom_hline(yintercept=0, linetype="solid", color = "black", size = .3) +
  #theme_bly_style() +
  labs(x = "Year",
       y = "Difference from Baseline",
       title = "Baseline Recovery Ratios",
       subtitle = NULL,
       caption = NULL) +
  theme(#axis.title.x.bottom = element_blank(),
    #axis.title.y.left = element_blank(),
    axis.title.y.right = element_blank(),
    axis.text.x.bottom = element_text(size = 5.5),
    #axis.text.y.left = element_text(),
    plot.caption = element_markdown(size = 6),
    axis.text.y.right = element_blank(),
    #panel.grid.major.y = element_line(),
    #panel.grid.minor.y = element_line(),
    legend.position = "bottom")
brr_lc
#------------------------------------------------------------------------------
baseline_recovery_ratios_asset_class <-  read.csv(paste0(getwd(), baseline_path,
                                             "totals/recovery_ratios_asset_class.csv"))

baseline_man_data <- baseline_recovery_ratios_asset_class %>%
  group_by(asset_class) %>%
  mutate(delta_real = ((real - first(real)) * 1),
         delta_pv = ((pv - first(pv)) * 1)) %>%
  select(year,asset_class, delta_real, delta_pv)

write.csv(baseline_man_data, paste0(getwd(), baseline_path,
                                    "deltas/recovery_ratios_asset_class.csv"))

scenario <- baseline_man_data %>%
  pivot_longer(cols = -c(year, asset_class),
               names_to = "scenario",
               values_to = "value")

brr_ac_lc <- ggplot(scenario) +
  geom_line(aes(x = year, y = value, color = asset_class), linewidth = 1) +
  geom_hline(yintercept=0, linetype="solid", color = "black", size = .3) +
  #theme_bly_style() +
  labs(x = "Year",
       y = "Difference from Baseline",
       title = "Baseline Recovery Ratios: Asset Class",
       subtitle = NULL,
       caption = NULL) +
  theme(#axis.title.x.bottom = element_blank(),
    #axis.title.y.left = element_blank(),
    axis.title.y.right = element_blank(),
    axis.text.x.bottom = element_text(size = 5.5),
    #axis.text.y.left = element_text(),
    plot.caption = element_markdown(size = 6),
    axis.text.y.right = element_blank(),
    #panel.grid.major.y = element_line(),
    #panel.grid.minor.y = element_line(),
    legend.position = "bottom")
brr_ac_lc
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
baseline_recovery_ratios_form <-  read.csv(paste0(getwd(), baseline_path,
                                                         "totals/recovery_ratios_form.csv"))

baseline_man_data <- baseline_recovery_ratios_form %>%
  group_by(form) %>%
  mutate(delta_real = ((real - first(real)) * 1),
         delta_pv = ((pv - first(pv)) * 1)) %>%
  select(year,form, delta_real, delta_pv)

write.csv(baseline_man_data, paste0(getwd(), baseline_path,
                                    "deltas/recovery_ratios_form.csv"))

scenario <- baseline_man_data %>%
  pivot_longer(cols = -c(year, form),
               names_to = "scenario",
               values_to = "value")

brr_rr_f_lc_ccorp <- scenario %>%
  filter(form == "ccorp") %>%
  ggplot() +
  geom_line(aes(x = year, y = value, color = scenario), linewidth = 1) +
  geom_hline(yintercept=0, linetype="solid", color = "black", size = .3) +
  #theme_bly_style() +
  labs(x = "Year",
       y = "Difference from Baseline",
       title = "Baseline Recovery Ratios: Form == CC",
       subtitle = NULL,
       caption = NULL) +
  theme(#axis.title.x.bottom = element_blank(),
    #axis.title.y.left = element_blank(),
    axis.title.y.right = element_blank(),
    axis.text.x.bottom = element_text(size = 5.5),
    #axis.text.y.left = element_text(),
    plot.caption = element_markdown(size = 6),
    axis.text.y.right = element_blank(),
    #panel.grid.major.y = element_line(),
    #panel.grid.minor.y = element_line(),
    legend.position = "bottom")
brr_rr_f_lc_ccorp

brr_rr_f_lc_pt <- scenario %>%
  filter(form != "ccorp") %>%
  ggplot() +
  geom_line(aes(x = year, y = value, color = scenario), linewidth = 1) +
  geom_hline(yintercept=0, linetype="solid", color = "black", size = .3) +
  #theme_bly_style() +
  labs(x = "Year",
       y = "Difference from Baseline",
       title = "Baseline Recovery Ratios: Form == PT",
       subtitle = NULL,
       caption = NULL) +
  theme(#axis.title.x.bottom = element_blank(),
    #axis.title.y.left = element_blank(),
    axis.title.y.right = element_blank(),
    axis.text.x.bottom = element_text(size = 5.5),
    #axis.text.y.left = element_text(),
    plot.caption = element_markdown(size = 6),
    axis.text.y.right = element_blank(),
    #panel.grid.major.y = element_line(),
    #panel.grid.minor.y = element_line(),
    legend.position = "bottom")
brr_rr_f_lc_pt
#------------------------------------------------------------------------------



#------------------------------------------------------------------------------
baseline_recovery_ratios_industry <-  read.csv(paste0(getwd(), baseline_path,
                                                  "totals/recovery_ratios_industry.csv"))

baseline_man_data <- baseline_recovery_ratios_industry %>%
  group_by(industry) %>%
  mutate(delta_real = ((real - first(real)) * 1),
         delta_pv = ((pv - first(pv)) * 1)) %>%
  select(year,industry, delta_real, delta_pv)

write.csv(baseline_man_data, paste0(getwd(), baseline_path,
                                    "deltas/recovery_ratios_industry.csv"))

scenario <- baseline_man_data %>%
  pivot_longer(cols = -c(year, industry),
               names_to = "scenario",
               values_to = "value")

brr_rr_i_lc <- ggplot(scenario) +
  geom_line(aes(x = year, y = value, color = industry), linewidth = 1) +
  geom_hline(yintercept=0, linetype="solid", color = "black", size = .3) +
  #theme_bly_style() +
  labs(x = "Year",
       y = "Difference from Baseline",
       title = "Baseline Recovery Ratios: Industry",
       subtitle = NULL,
       caption = NULL) +
  theme(#axis.title.x.bottom = element_blank(),
    #axis.title.y.left = element_blank(),
    axis.title.y.right = element_blank(),
    axis.text.x.bottom = element_text(size = 5.5),
    #axis.text.y.left = element_text(),
    plot.caption = element_markdown(size = 6),
    axis.text.y.right = element_blank(),
    #panel.grid.major.y = element_line(),
    #panel.grid.minor.y = element_line(),
    legend.position = "bottom")
brr_rr_i_lc
#------------------------------------------------------------------------------

#----Export

p1 <- cowplot::plot_grid(baseline_by_deduction_lc_ccorp, baseline_by_deduction_lc_pt,
                         brr_lc,
                         brr_rr_f_lc_ccorp, brr_rr_f_lc_pt,
                         ncol = 2, nrow = 3)
p2 <- cowplot::plot_grid(brr_ac_lc,
                         NULL,
                         NULL,
                         ncol = 1, nrow = 1)
p3 <- cowplot::plot_grid(brr_rr_i_lc,
                         NULL,
                         NULL,
                         ncol = 1, nrow = 1)

pdf("./resources/output/2024101612/baseline/charts/baseline_diff.pdf", height = 11, width = 8.5, paper = "letter")
p1
p2
p3
dev.off()


#-------------------------------------EXPENSES 2023---------------------------#

############################################################################
#                               EXPENSES TEST                              #
############################################################################
#------------------------------------------------------------------------------
expenses_by_deduction_year <-  read.csv(paste0(getwd(), expenses_path,
                                               "totals/by_deduction_year.csv"))

expenses_man_data <- expenses_by_deduction_year %>%
  group_by(form) %>%
  mutate(delta_depreciation = ((depreciation - first(depreciation)) * 1),
         delta_nol = ((nol - first(nol)) * 1),
         delta_total = ((total - first(total)) * 1)) %>%
  select(form, deduction_year, delta_depreciation, delta_nol, delta_total)

write.csv(expenses_man_data, paste0(getwd(), expenses_path,
                                    "deltas/by_deduction_year.csv"))

scenario <- expenses_man_data %>%
  pivot_longer(cols = -c(form ,deduction_year),
               names_to = "scenario",
               values_to = "value")

baseline_by_deduction_lc_ccorp <- scenario %>%
  filter(form == "ccorp") %>%
  ggplot() +
  geom_line(aes(x = deduction_year, y = value, color = scenario), linewidth = 1) +
  geom_hline(yintercept=0, linetype="solid", color = "black", size = .3) +
  #theme_bly_style() +
  labs(x = "Year",
       y = "Difference from Baseline",
       title = "EXP by Deduction: Form == CC",
       subtitle = NULL,
       caption = NULL) +
  theme(#axis.title.x.bottom = element_blank(),
    #axis.title.y.left = element_blank(),
    axis.title.y.right = element_blank(),
    axis.text.x.bottom = element_text(size = 5.5),
    #axis.text.y.left = element_text(),
    plot.caption = element_markdown(size = 6),
    axis.text.y.right = element_blank(),
    #panel.grid.major.y = element_line(),
    #panel.grid.minor.y = element_line(),
    legend.position = "bottom")
baseline_by_deduction_lc_ccorp

baseline_by_deduction_lc_pt <- scenario %>%
  filter(form != "ccorp") %>%
  ggplot() +
  geom_line(aes(x = deduction_year, y = value, color = scenario), linewidth = 1) +
  geom_hline(yintercept=0, linetype="solid", color = "black", size = .3) +
  #theme_bly_style() +
  labs(x = "Year",
       y = "Difference from Baseline",
       title = "EXP by Deduction: Form == PT",
       subtitle = NULL,
       caption = NULL) +
  theme(#axis.title.x.bottom = element_blank(),
    #axis.title.y.left = element_blank(),
    axis.title.y.right = element_blank(),
    axis.text.x.bottom = element_text(size = 5.5),
    #axis.text.y.left = element_text(),
    plot.caption = element_markdown(size = 6),
    axis.text.y.right = element_blank(),
    #panel.grid.major.y = element_line(),
    #panel.grid.minor.y = element_line(),
    legend.position = "bottom")
baseline_by_deduction_lc_pt
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
expenses_recovery_ratios <-  read.csv(paste0(getwd(), expenses_path,
                                             "totals/recovery_ratios.csv"))

expenses_man_data <- expenses_recovery_ratios %>%
  mutate(delta_real = ((real - first(real)) * 1),
         delta_pv = ((pv - first(pv)) * 1)) %>%
  select(year, delta_real, delta_pv)

write.csv(expenses_man_data, paste0(getwd(), expenses_path,
                                    "deltas/recovery_ratios.csv"))
scenario <- expenses_man_data %>%
  pivot_longer(cols = -c(year),
               names_to = "scenario",
               values_to = "value")

brr_lc <- ggplot(scenario) +
  geom_line(aes(x = year, y = value, color = scenario), linewidth = 1) +
  geom_hline(yintercept=0, linetype="solid", color = "black", size = .3) +
  #theme_bly_style() +
  labs(x = "Year",
       y = "Difference from Baseline",
       title = "Expenses2023 Recovery Ratios",
       subtitle = NULL,
       caption = NULL) +
  theme(#axis.title.x.bottom = element_blank(),
    #axis.title.y.left = element_blank(),
    axis.title.y.right = element_blank(),
    axis.text.x.bottom = element_text(size = 5.5),
    #axis.text.y.left = element_text(),
    plot.caption = element_markdown(size = 6),
    axis.text.y.right = element_blank(),
    #panel.grid.major.y = element_line(),
    #panel.grid.minor.y = element_line(),
    legend.position = "bottom")
brr_lc

#------------------------------------------------------------------------------
expenses_recovery_ratios_asset_class <-  read.csv(paste0(getwd(), expenses_path,
                                                         "totals/recovery_ratios_asset_class.csv"))

expenses_man_data <- expenses_recovery_ratios_asset_class %>%
  group_by(asset_class) %>%
  mutate(delta_real = ((real - first(real)) * 1),
         delta_pv = ((pv - first(pv)) * 1)) %>%
  select(year,asset_class, delta_real, delta_pv)

write.csv(expenses_man_data, paste0(getwd(), expenses_path,
                                    "deltas/recovery_ratios_asset_class.csv"))

scenario <- expenses_man_data %>%
  pivot_longer(cols = -c(year, asset_class),
               names_to = "scenario",
               values_to = "value")

brr_ac_lc <- ggplot(scenario) +
  geom_line(aes(x = year, y = value, color = asset_class), linewidth = 1) +
  geom_hline(yintercept=0, linetype="solid", color = "black", size = .3) +
  #theme_bly_style() +
  labs(x = "Year",
       y = "Difference from Baseline",
       title = "Expenses2023 Recovery Ratios: Asset Class",
       subtitle = NULL,
       caption = NULL) +
  theme(#axis.title.x.bottom = element_blank(),
    #axis.title.y.left = element_blank(),
    axis.title.y.right = element_blank(),
    axis.text.x.bottom = element_text(size = 5.5),
    #axis.text.y.left = element_text(),
    plot.caption = element_markdown(size = 6),
    axis.text.y.right = element_blank(),
    #panel.grid.major.y = element_line(),
    #panel.grid.minor.y = element_line(),
    legend.position = "bottom")
brr_ac_lc
#----------
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
expenses_recovery_ratios_form <-  read.csv(paste0(getwd(), expenses_path,
                                                  "totals/recovery_ratios_form.csv"))

expenses_man_data <- expenses_recovery_ratios_form %>%
  group_by(form) %>%
  mutate(delta_real = ((real - first(real)) * 1),
         delta_pv = ((pv - first(pv)) * 1)) %>%
  select(year,form, delta_real, delta_pv)

write.csv(expenses_man_data, paste0(getwd(), expenses_path,
                                    "deltas/recovery_ratios_form.csv"))

scenario <- expenses_man_data %>%
  pivot_longer(cols = -c(year, form),
               names_to = "scenario",
               values_to = "value")

brr_rr_f_lc_ccorp <- scenario %>%
  filter(form == "ccorp") %>%
  ggplot() +
  geom_line(aes(x = year, y = value, color = scenario), linewidth = 1) +
  geom_hline(yintercept=0, linetype="solid", color = "black", size = .3) +
  #theme_bly_style() +
  labs(x = "Year",
       y = "Difference from Baseline",
       title = "EXP Recovery Ratios: Form == CC",
       subtitle = NULL,
       caption = NULL) +
  theme(#axis.title.x.bottom = element_blank(),
    #axis.title.y.left = element_blank(),
    axis.title.y.right = element_blank(),
    axis.text.x.bottom = element_text(size = 5.5),
    #axis.text.y.left = element_text(),
    plot.caption = element_markdown(size = 6),
    axis.text.y.right = element_blank(),
    #panel.grid.major.y = element_line(),
    #panel.grid.minor.y = element_line(),
    legend.position = "bottom")
brr_rr_f_lc_ccorp

brr_rr_f_lc_pt <- scenario %>%
  filter(form != "ccorp") %>%
  ggplot() +
  geom_line(aes(x = year, y = value, color = scenario), linewidth = 1) +
  geom_hline(yintercept=0, linetype="solid", color = "black", size = .3) +
  #theme_bly_style() +
  labs(x = "Year",
       y = "Difference from Baseline",
       title = "EXP Recovery Ratios: Form == PT",
       subtitle = NULL,
       caption = NULL) +
  theme(#axis.title.x.bottom = element_blank(),
    #axis.title.y.left = element_blank(),
    axis.title.y.right = element_blank(),
    axis.text.x.bottom = element_text(size = 5.5),
    #axis.text.y.left = element_text(),
    plot.caption = element_markdown(size = 6),
    axis.text.y.right = element_blank(),
    #panel.grid.major.y = element_line(),
    #panel.grid.minor.y = element_line(),
    legend.position = "bottom")
brr_rr_f_lc_pt

#------------------------------------------------------------------------------



#------------------------------------------------------------------------------
expenses_recovery_ratios_industry <-  read.csv(paste0(getwd(), expenses_path,
                                                      "totals/recovery_ratios_industry.csv"))

expenses_man_data <- expenses_recovery_ratios_industry %>%
  group_by(industry) %>%
  mutate(delta_real = ((real - first(real)) * 1),
         delta_pv = ((pv - first(pv)) * 1)) %>%
  select(year,industry, delta_real, delta_pv)

write.csv(expenses_man_data, paste0(getwd(), expenses_path,
                                    "deltas/recovery_ratios_industry.csv"))

scenario <- expenses_man_data %>%
  pivot_longer(cols = -c(year, industry),
               names_to = "scenario",
               values_to = "value")

brr_rr_i_lc <- ggplot(scenario) +
  geom_line(aes(x = year, y = value, color = industry), linewidth = 1) +
  geom_hline(yintercept=0, linetype="solid", color = "black", size = .3) +
  #theme_bly_style() +
  labs(x = "Year",
       y = "Difference from Baseline",
       title = "Expenses2023 Recovery Ratios: Industry",
       subtitle = NULL,
       caption = NULL) +
  theme(#axis.title.x.bottom = element_blank(),
    #axis.title.y.left = element_blank(),
    axis.title.y.right = element_blank(),
    axis.text.x.bottom = element_text(size = 5.5),
    #axis.text.y.left = element_text(),
    plot.caption = element_markdown(size = 6),
    axis.text.y.right = element_blank(),
    #panel.grid.major.y = element_line(),
    #panel.grid.minor.y = element_line(),
    legend.position = "bottom")
brr_rr_i_lc
#------------------------------------------------------------------------------
#Export
#------------------------------------------------------------------------------

#----Export

p1 <- cowplot::plot_grid(baseline_by_deduction_lc_ccorp, baseline_by_deduction_lc_pt,
                         brr_lc,
                         brr_rr_f_lc_ccorp, brr_rr_f_lc_pt,
                         ncol = 2, nrow = 3)
p2 <- cowplot::plot_grid(brr_ac_lc,
                         NULL,
                         NULL,
                         ncol = 1, nrow = 1)
p3 <- cowplot::plot_grid(brr_rr_i_lc,
                         NULL,
                         NULL,
                         ncol = 1, nrow = 1)

pdf("./resources/output/2024101612/expensing2023/charts/expensing2023_diff.pdf", height = 11, width = 8.5, paper = "letter")
p1
p2
p3
dev.off()


#----------------------------------------------------------------------------
#Charting

# create_val_line <- function(var_name){
#   #----Which Scenario?
#   # if(scenario_name == "baseline"){
#   #   scenario_data <- baseline_man_data
#   # } else if(scenario_name == "full"){
#   #   scenario_data <- full_ext_man_data
#   # } else if(scenario_name == "partial"){
#   #   scenario_data <- partial_ext_man_data
#   # } else if(scenario_name == "clausing-sarin"){
#   #   scenario_data <- sc_ext_man_data
#   # }
#   #-------------------
#   #----Which variable?
#   if(var_name == "xgdp"){
#     var_data <- paste0(c("pct_change_xgdp"))
#     subtitle_name <- "Percent"
#     title_name <- "Real Gross Domestic Product"
#   } else if(var_name == "xgdpt"){
#     var_data <- paste0(c("pct_change_xgdpt"))
#     subtitle_name <- "Percent"
#     title_name <- "Real Potential Gross Domestic Product"
#   } else if(var_name == "trci"){
#     var_data <- paste0(c("pct_change_trci"))
#     subtitle_name <- "Percentage Point"
#     title_name <- "Average Corporate Income Tax Rate"
#   } else if(var_name == "trfcim"){
#     var_data <- paste0(c("pct_change_trfcim"))
#     subtitle_name <- "Percentage Point"
#     title_name <- "Marginal Corporate Income Tax Rate"
#   } else if(var_name == "xgdpt"){
#     var_data <- paste0(c("pct_change_xgdpt"))
#     subtitle_name <- "Percent"
#     title_name <- "Real Potential Gross Domestic Product"
#   } else if(var_name == "lww"){
#     var_data <- paste0(c("pct_change_lww"))
#     subtitle_name <- "Percent"
#     title_name <- "Workweek, Business Sector"
#   } else if(var_name == "mfpt"){
#     var_data <- paste0(c("pct_change_mfpt"))
#     subtitle_name <- "Percent"
#     title_name <- "Multifactor Productivity, Trend Level"
#   } else if(var_name == "rtbfi"){
#     var_data <- paste0(c("pct_change_rtbfi"))
#     subtitle_name <- "Percent"
#     title_name <- "Real User Cost of Capital for BFI"
#   } else if(var_name == "ebfi"){
#     var_data <- paste0(c("pct_change_ebfi"))
#     subtitle_name <- "Percent"
#     title_name <- "Real Business Fixed Investment"
#   } else if(var_name == "kbfi"){
#     var_data <- paste0(c("pct_change_kbfi"))
#     subtitle_name <- "Percent"
#     title_name <- "Business Fixed Investment Capital Stock"
#   } else if(var_name == "ks"){
#     var_data <- paste0(c("pct_change_ks"))
#     subtitle_name <- "Percent"
#     title_name <- "Capital Services"
#   } else if(var_name == "xgap2"){
#     var_data <- paste0(c("pct_change_xgap2"))
#     subtitle_name <- "Percentage Point"
#     title_name <- "Output Gap"
#   } else if(var_name == "lfpr"){
#     var_data <- paste0(c("pct_change_lfpr"))
#     subtitle_name <- "Percentage Point"
#     title_name <- "Labor Force Participation Rate"
#   } else if(var_name == "yniln_ynin"){
#     var_data <- paste0(c("pct_change_yniln_ynin"))
#     subtitle_name <- "Percentage Point"
#     title_name <- "Labor Share of Income"
#   }
#   #-------------------
#   scenario_data <- total %>%
#     filter(variable == var_data)
#   
#   scenario_data$date <- as.yearqtr(scenario_data$YearQtr)
#   scenario_data <- scenario_data %>%
#     filter(YearQtr >= as.yearqtr(as.Date("2025-01-01")))
#   #---Xaxis
#   #xstart <- as.yearqtr(as.Date("2025-07-01"))
#   #xend <- as.yearqtr(as.Date("2055-07-01"))
#   #------
#   
#   #---Y-axis
#   if(var_name == "xgdp"){
#     ymin <- round_any(min(scenario_data$value), .5, f = floor)
#     ymax <- round_any(max(scenario_data$value), .5, f =  ceiling)
#     ydelta <- .1
#   } else if(var_name == "xgdpt"){
#     ymin <- round_any(min(scenario_data$value), .5, f = floor)
#     ymax <- round_any(max(scenario_data$value), .5, f =  ceiling)
#     ydelta <- .1
#   } else if(var_name == "trci"){
#     ymin <- round_any(min(scenario_data$value), 1, f = floor)
#     ymax <- round_any(max(scenario_data$value), 1, f =  ceiling)
#     ydelta <- 1
#   } else if(var_name == "trfcim"){
#     ymin <- round_any(min(scenario_data$value), 1, f = floor)
#     ymax <- round_any(max(scenario_data$value), 1, f =  ceiling)
#     ydelta <- 1
#   } else if(var_name == "lww"){
#     ymin <- round_any(min(scenario_data$value), .1, f = floor)
#     ymax <- round_any(max(scenario_data$value), .1, f =  ceiling)
#     ydelta <- .05
#   } else if(var_name == "rtbfi"){
#     ymin <- -1
#     ymax <- .5
#     ydelta <- .5
#   } else if(var_name == "mfpt"){
#     ymin <- -.5
#     ymax <- .5
#     ydelta <- .1
#   } else if(var_name == "ebfi"){
#     ymin <- round_any(min(scenario_data$value), 1, f = floor)
#     ymax <- round_any(max(scenario_data$value), 1, f =  ceiling)
#     ydelta <- 1
#   } else if(var_name == "kbfi"){
#     ymin <- round_any(min(scenario_data$value), 1, f = floor)
#     ymax <- round_any(max(scenario_data$value), 1, f =  ceiling)
#     ydelta <- 1
#   } else if(var_name == "ks"){
#     ymin <- round_any(min(scenario_data$value), 1, f = floor)
#     ymax <- round_any(max(scenario_data$value), 1, f =  ceiling)
#     ydelta <- 1
#   } else if(var_name == "xgap2"){
#     ymin <- round_any(min(scenario_data$value), 10, f = floor)
#     ymax <- round_any(max(scenario_data$value), 10, f =  ceiling)
#     ydelta <- 10
#   } else if(var_name == "lfpr"){
#     ymin <- round_any(min(scenario_data$value), .05, f = floor)
#     ymax <- round_any(max(scenario_data$value), .05, f =  ceiling)
#     ydelta <- .01
#   } else if(var_name == "yniln_ynin"){
#     ymin <- round_any(min(scenario_data$value), .5, f = floor)
#     ymax <- round_any(max(scenario_data$value), .5, f =  ceiling)
#     ydelta <- .1
#   }
#   #---------
#   #Chart It!
#   line_chart <- ggplot(scenario_data %>% filter(scenario != "Baseline")) +
#     geom_line(aes(x = date, y = value, color = scenario), linewidth = 1) +
#     geom_hline(yintercept=0, linetype="solid", color = "black", size = .3) +
#     #theme_bly_style() +
#     scale_color_manual(values = c("Baseline" = "black",
#                                   "Full Extension" = "dodgerblue",
#                                   "Partial Extension" = "goldenrod",
#                                   "Sarin-Clausing" = "mediumorchid4")) +
#     labs(x = "Year-Q",
#          y = "Difference from Baseline",
#          title = title_name,
#          subtitle = subtitle_name,
#          caption = NULL) +
#     theme(#axis.title.x.bottom = element_blank(),
#       #axis.title.y.left = element_blank(),
#       axis.title.y.right = element_blank(),
#       axis.text.x.bottom = element_text(size = 5.5),
#       #axis.text.y.left = element_text(),
#       plot.caption = element_markdown(size = 6),
#       axis.text.y.right = element_blank(),
#       #panel.grid.major.y = element_line(),
#       #panel.grid.minor.y = element_line(),
#       legend.position = "bottom") +
#     scale_y_continuous(sec.axis = dup_axis(),
#                        limits = c(ymin, ymax),
#                        n.breaks = 5,
#                        expand = c(0,0)) +
#     scale_x_yearqtr(n = 10)
#   line_chart
#   
# }
# 
# #-----
# var_name <- "trci"
# trci_graph <- create_val_line(var_name)
# trci_graph
# 
# var_name <- "trfcim"
# trfcim_graph <- create_val_line(var_name)
# trfcim_graph
# 
# var_name <- "xgdp"
# xgdp_graph <- create_val_line(var_name)
# xgdp_graph
# 
# var_name <- "xgdpt"
# xgdpt_graph <- create_val_line(var_name)
# xgdpt_graph
# 
# var_name <- "lww"
# lww_graph <- create_val_line(var_name)
# lww_graph
# 
# # var_name <- "mfpt"
# # mfpt_graph <- create_val_line(var_name)
# # mfpt_graph
# 
# var_name <- "rtbfi"
# rtbfi_graph <- create_val_line(var_name)
# rtbfi_graph
# 
# var_name <- "ebfi"
# ebfi_graph <- create_val_line(var_name)
# ebfi_graph
# 
# var_name <- "kbfi"
# kbfi_graph <- create_val_line(var_name)
# kbfi_graph
# 
# var_name <- "ks"
# ks_graph <- create_val_line(var_name)
# ks_graph
# 
# var_name <- "xgap2"
# xgap2_graph <- create_val_line(var_name)
# xgap2_graph
# 
# var_name <- "lfpr"
# lfpr_graph <- create_val_line(var_name)
# lfpr_graph
# 
# var_name <- "yniln_ynin"
# yniln_ynin_graph <- create_val_line(var_name)
# yniln_ynin_graph
# 
# 
# #----Export
# 
# p1 <- cowplot::plot_grid(trci_graph, trfcim_graph,
#                          xgdp_graph, xgdpt_graph,
#                          lww_graph, rtbfi_graph,
#                          ncol = 2, nrow = 3)
# p2 <- cowplot::plot_grid(ebfi_graph, kbfi_graph,
#                          ks_graph, xgap2_graph,
#                          lfpr_graph, yniln_ynin_graph,
#                          ncol = 2, nrow = 3)
# 
# pdf("/Users/dylansaez/Desktop/FOLDERS/R/BLY/charts_for_ken/output/output.pdf", height = 11, width = 8.5, paper = "letter")
# p1
# p2
# dev.off()
#
