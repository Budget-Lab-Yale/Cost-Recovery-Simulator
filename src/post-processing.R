#---------------------------------------------------
# Post-processing.R
# 
# Contains functions to process detailed deductions
# file for summary output
#---------------------------------------------------




calc_recovery_ratios = function(scenario_info, depreciation_detailed, 
                                macro_projections, assumptions, group_var, 
                                spread = 0.02) {
  
  #----------------------------------------------------------------------------
  # Calculates the ratio of the value of depreciation deductions (both real  
  # and present value) to investment basis by year X specified grouping var.
  # 
  # Parameters:
  # - scenario_info       (list) : scenario info object 
  #                                (see get_scenario_info())
  # - depreciation_detailed (df) : tibble of deductions by asset class, long 
  #                                in investment year and wide in deduction 
  #                                year (see calc_all_depreciation()) 
  # - macro_projections     (df) : tibble of macro variable projections
  # - assumptions         (list) : assumptions object (see build_assumptions())
  # - group_var            (obj) : grouping variable
  # - spread               (dbl) : assumed discount rate spread over 10-year
  # 
  # Returns:
  # - void (writes output)
  #----------------------------------------------------------------------------
  
  # Generate file name dynamically 
  if (rlang::as_label(enquo(group_var)) == 'NULL') { 
    file_name = 'recovery_ratios.csv' 
  } else {
    file_name = paste0('recovery_ratios_', rlang::as_name(enquo(group_var)), '.csv')
  }
  
  
  depreciation_detailed %>% 
    
    # Aggregate before reshaping long (for memory)
    group_by(year, {{ group_var }}) %>% 
    summarise(
      across(
        .cols = c(investment, matches('[[:digit:]]')), 
        .fns  = sum
      ), 
      .groups = 'drop'
    ) %>% 
    
    # Get investment year X deduction year totals
    pivot_longer(
      cols            = matches('[[:digit:]]'), 
      names_to        = 'deduction_year', 
      names_transform = as.integer,
      values_to       = 'deductions'
    ) %>% 
    filter(deduction_year >= year, deduction_year <= max(scenario_info$years)) %>% 
    
    # Join risk-free rate and calculate discount rate
    left_join(
      macro_projections %>% 
        mutate(
          inflation_rate = cpiu / lag(cpiu) - 1, 
          discount_rate  = tsy_10y / 100 + spread
        ) %>% 
        select(year, inflation_rate, discount_rate), 
      by = c('deduction_year' = 'year')
    ) %>% 
    
    # Calculate present value of deductions
    group_by(year, {{ group_var }}) %>% 
    summarise(
      investment = unique(investment),
      real       = sum(deductions / cumprod(1 + lag(inflation_rate, default = 0))),
      pv         = sum(deductions / cumprod(1 + lag(discount_rate,  default = 0))), 
      .groups = 'drop'
    ) %>% 
    
    # Calculate lifetime recovery ratio
    mutate(across(.cols = c(real, pv), .fns = ~ . / investment)) %>% 
    select(-investment) %>% 
    write_csv(
      file.path(scenario_info$paths$output, 'totals', file_name)
    )
}


#----Graphing
graphing <- function(csv_name, cols_to_group_by) { 
  
  baseline <-  read.csv(paste0(getwd(),
                               baseline_path,
                               paste0("totals/", csv_name, ".csv"))) %>%
    pivot_longer(cols = -c(cols_to_group_by),
                 names_to = "scenario",
                 values_to = "value") %>%
    rename_with(~ paste0("baseline_", .), -c(cols_to_group_by, scenario))
  
  non_baseline <- read.csv(paste0(getwd(), expenses_path,
                                  paste0("totals/", csv_name, ".csv"))) %>%
    pivot_longer(cols = -c(cols_to_group_by),
                 names_to = "scenario",
                 values_to = "value") %>%
    rename_with(~ paste0("exp2023_", .), -c(cols_to_group_by, scenario))
  
  merged_df <- merge(baseline, non_baseline,
                     by = c(cols_to_group_by, "scenario"),
                     all = TRUE)
  merged_df <- merged_df %>%
    group_by(merged_df[,names(merged_df) %in% c(cols_to_group_by, "scenario")]) %>%
    mutate(delta = exp2023_value - baseline_value)
  
  #Write back to delta folders, but in wide form!
  write_csv(merged_df %>%
              select(-c("exp2023_value")) %>%
              pivot_wider(names_from = scenario, values_from = baseline_value),
            paste0(getwd(), baseline_path,
                   paste0("deltas/", csv_name, ".csv")))
  write_csv(merged_df %>%
              select(-c("baseline_value")) %>%
              pivot_wider(names_from = scenario, values_from = exp2023_value),
            paste0(getwd(), expenses_path,
                   paste0("deltas/", csv_name, ".csv")))
  
  print("Delta CSVs have been published to /deltas/ folder in all scenario folders")
  #
  
  print("Now graphing...")
  
  #Change all columns that contain the name year to just year..
  colnames(merged_df)[grepl('year',colnames(merged_df))] <- 'year'
  #
  
  if (length(cols_to_group_by) > 1) {
    
    cols_to_group_by_ref <- cols_to_group_by[!str_detect(cols_to_group_by,
                                                         pattern="year")]
    for (j in 1:length(cols_to_group_by_ref)) {
      choices_i <- unique(merged_df[,cols_to_group_by_ref][j])
      print(paste0("The number of graphs for this data will be: ", n_distinct(merged_df[,cols_to_group_by_ref][j])))
      
      for (k in 1:n_distinct(choices_i)){
        
        for (i in 1:length(cols_to_group_by_ref)) {
          
          graph <-
            filter(merged_df, !!parse_expr(cols_to_group_by_ref[i]) == toString(pull(choices_i)[k])) %>%
            ggplot() +
            geom_line(aes(x =  year, y = delta, color = scenario), linewidth = 1) +
            geom_hline(yintercept=0, linetype="solid", color = "black", size = .3) +
            #theme_bly_style() +
            labs(x = "Year",
                 y = "Exp2023 - Baseline",
                 title = paste0("Delta from Baseline: Form == ", toString(pull(choices_i)[k])),
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
          png(filename = paste0("/gpfs/gibbs/project/sarin/ds3228/Repositories/Depreciation/resources/output/", time_stamp, "/chart_packs/",toString(pull(choices_i)[k]),".png"))
          print(graph)
          dev.off()
        }
      }
    }
  } else{
    
    graph <- merged_df %>%
      #filter(merged_df, !!parse_expr(cols_to_group_by_ref[i]) == toString(pull(choices_i)[k])) %>%
      ggplot() +
      geom_line(aes(x =  year, y = delta, color = scenario), linewidth = 1) +
      geom_hline(yintercept=0, linetype="solid", color = "black", size = .3) +
      #theme_bly_style() +
      labs(x = "Year",
           y = "Exp2023 - Baseline",
           title = paste0("Delta from Baseline"),
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
    png(filename = paste0("/gpfs/gibbs/project/sarin/ds3228/Repositories/Depreciation/resources/output/", time_stamp, "/chart_packs/",csv_name,".png"))
    print(graph)
    dev.off()
  }
}
