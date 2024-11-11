


across_scenario_calculating_and_graphing = function(){
  
  #Step .01 - Collect the names of the dimensions of the data.
  #All_csvs is a nomer for the types of data we have.
  
  baseline_path <- paste0(sub('.', '', platform_defaults$roots$output),"/",time_stamp,"/baseline/")
  all_csvs <- as.data.frame(list.files(paste0(getwd(), sub('.', '', platform_defaults$roots$output), "/",time_stamp, "/baseline/totals")))
  
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
  
  #Up to this point we have the names of the attributes of the data buckets we have.
  # Step .02 - Collect the second dimension of our data - all the scenario 
  #  whereby the buckets of our data repeat over - if we have 3 scenarios including
  #  baseline, then the # of all_csvs we have multiplied by the number of scenarios
  #  we have is 3# in this case.
  all_scenarios <- as.data.frame(list.files(paste0(getwd(), sub('.', '', platform_defaults$roots$output),"/", time_stamp)))
  colnames(all_scenarios) <- "scenario_names"
  
  scenario_path_list <- list()
  for (scenario in 1:nrow(all_scenarios)){
    scenario_path <- paste0(getwd(), sub('.', '', platform_defaults$roots$output),"/", time_stamp, "/", all_scenarios$scenario_names[scenario], "/")
    scenario_path_list <- append(scenario_path, scenario_path_list)
    
  }
  all_scenarios$file_path <- unlist(rev(scenario_path_list))
  

  #Step .03
  #Now that we have the filepaths and names of the data we will be calculating
  # and graphing, let's open all this data up before anything else...
  for (csv in 1:nrow(all_csvs)){
    for (scen in 1:nrow(all_scenarios)){
      assign(paste(all_scenarios$scenario_names[scen],all_csvs$csv_names[csv], sep = "-"),
             read.csv(paste0(all_scenarios$file_path[scen],
                             paste0("totals/", all_csvs$csv_names[csv], ".csv"))) %>%
               pivot_longer(cols = -c(strsplit(all_csvs$cols_to_group_by[csv], ",")[[1]]),
                            names_to = "scenario",
                            values_to = "value") %>%
               rename_with(~ paste0(all_scenarios$scenario_names[scen],"_", .), -c(strsplit(all_csvs$cols_to_group_by[csv], ",")[[1]], scenario)))
    }
    #print("We have opened all CSVs. Now topically merging together...")
  }
  
  
  #Step .04
  # Let's merge together all csvs across all scenarios.
  # This will reduce our dimensions down to the number of all_csvs.
  
  for(q in 1:nrow(all_csvs)){
    df_list <- mget(ls(pattern = paste0("-",all_csvs$csv_names[q])))
    assign(paste0(all_csvs$csv_names[q], "_merged_df"), df_list %>% reduce(inner_join, by = c(strsplit(all_csvs$cols_to_group_by[q], ",")[[1]], "scenario")))
    rm(list=ls(pattern=paste0("-",all_csvs$csv_names[q])))
    
  }
  

  merged_df_list <- mget(ls(pattern = paste0("_","merged_df")))

  #Step .05 - Some minor house cleaning
for(trial in 1:length(merged_df_list)){
  intermediate_01 <- as.data.frame(merged_df_list[trial])
  number_of_position <- as.integer(trial)
  for (col in 1:ncol(intermediate_01)){
    colnames(intermediate_01)[col] <-  sub(".*_merged_df.", "", colnames(intermediate_01)[col])
  }

  intermediate_01$year <- lubridate::ymd(intermediate_01$year, truncated = 2L)

  colnames(intermediate_01)[grepl('baseline_value',colnames(intermediate_01))] <- 'baseline_value'
  
  
  #Step .06
  #Pivot the merged data longer in order to calculate scenario from baseline
  #  for all non-baseline scenarios.
  intermediate_02 <- intermediate_01 %>%
    group_by(intermediate_01[,names(intermediate_01) %in% c(strsplit(all_csvs$cols_to_group_by[number_of_position], ",")[[1]], "scenario")]) %>%
    mutate(across(#Select columns
      -c(baseline_value),
      #lambda
      ~.x - baseline_value,
      .names = "{.col}_delta_from_baseline")) %>%
    pivot_longer(cols = contains("value"),
                 names_to = "variable",
                 values_to = "value")
  
  assign(paste0(all_csvs$csv_names[number_of_position], "_clean_long_df"), intermediate_02)
  
  intermediate_02_wide <- intermediate_02 %>%
    pivot_wider(names_from = scenario, values_from = value)
  
  assign(paste0(all_csvs$csv_names[number_of_position], "_clean_WIDE_df"), intermediate_02_wide)

  
  long_dataframes_list <- mget(ls(pattern = paste0("_","clean_long_df")))


  wide_dataframes_calculated_list <- mget(ls(pattern = paste0("_","clean_WIDE_df")))
}


#---Some house cleaning to ensure that R does not confused the name of scenarios
  # regardless of the special characters they may have in the names.
  all_scenarios$name_dot <- all_scenarios$scenario_names %>%
    str_replace_all("-", ".")
  all_scenarios <- all_scenarios %>%
    filter(!grepl("baseline", scenario_names))
  
  all_csvs <- all_csvs[match(sub("_clean_long_df", "", names(long_dataframes_list)), all_csvs$csv_names),]
  
#Step .07
  # Now, for each scenario-csv type, graph scenario from baseline for each
  # unique characteristics within the data
  # Whether the data has columns that specify the data or not, this function
  # will print out as many slices of the data as needed.
for(trial_time in 1:length(long_dataframes_list)){
    graphing_df_01 <- as.data.frame(long_dataframes_list[trial_time])
    deedee <- as.integer(trial_time)
    for (colu in 1:ncol(graphing_df_01)){
      colnames(graphing_df_01)[colu] <-  sub(".*_clean_long_df.", "", colnames(graphing_df_01)[colu])
    }

  graphing_df_01$year <- lubridate::ymd(graphing_df_01$year, truncated = 2L)

    for(eekeek in 1:nrow(all_scenarios)){
      cols_to_group_by <- strsplit(all_csvs$cols_to_group_by[deedee], ",")[[1]]
      
      if (length(cols_to_group_by) > 1) {
        
        cols_to_group_by_ref <- cols_to_group_by[!str_detect(cols_to_group_by,
                                                             pattern="year")]
        
        choices_i <- unique(graphing_df_01[,cols_to_group_by_ref])
        
        for (k in 1:n_distinct(choices_i)){
          for (i in 1:length(cols_to_group_by_ref)) {
            
            graph <- graphing_df_01 %>%
              filter(!!parse_expr(cols_to_group_by_ref[i]) == choices_i[k]) %>%
              filter(grepl("delta", variable)) %>%
              filter(grepl(all_scenarios$name_dot[eekeek], variable)) %>%
              ggplot() +
              geom_line(aes(x =  year, y = value, color = scenario), linewidth = 1) +
              geom_hline(yintercept=0, linetype="solid", color = "black", linewidth = .3) +
              labs(x = "Year",
                   y = "Difference from Baseline",
                   title = paste(str_to_title(all_scenarios$scenario_names[eekeek]), "- Baseline"),
                   subtitle = NULL,
                   caption = NULL) +
              theme_light() +
              theme(#axis.title.x.bottom = element_blank(),
                #axis.title.y.left = element_blank(),
                axis.title.y.right = element_blank(),
                plot.title = element_text(size = 18),
                axis.title.y.left = element_text(size = 16),
                axis.title.x.bottom = element_text(size = 16),
                axis.text.x.bottom = element_text(size = 14),
                axis.text.y.left = element_text(size = 14),
                plot.caption = element_markdown(size = 14),
                axis.text.y.right = element_blank(),
                #panel.grid.major.y = element_line(),
                #panel.grid.minor.y = element_line(),
                legend.position = "bottom",
                legend.title = element_text(size = 14),
                legend.text = element_text(size = 14)) +
              guides(color=guide_legend(title="Metric")) +
              scale_color_brewer(palette="Dark2")
            
            #Export the graph to the scenario-specific folder in wd()
            png(filename = paste0(getwd(),sub('.', '', platform_defaults$roots$output),"/", time_stamp,"/", all_scenarios$scenario_names[eekeek],
                                  "/chart_packs/",choices_i[k],".png"))
            print(graph)
            dev.off()
          }
        }
      } else{
        graph <- graphing_df_01 %>%
          filter(grepl("delta", variable)) %>%
          filter(grepl(all_scenarios$name_dot[eekeek], variable)) %>%
          ggplot() +
          geom_line(aes(x =  year, y = value, color = scenario), linewidth = 1) +
          geom_hline(yintercept=0, linetype="solid", color = "black", linewidth = .3) +
          labs(x = "Year",
               y = "Difference from Baseline",
               title = paste(str_to_title(all_scenarios$scenario_names[eekeek]), "- Baseline"),
               subtitle = NULL,
               caption = NULL) +
          theme_light() +
          theme(#axis.title.x.bottom = element_blank(),
            #axis.title.y.left = element_blank(),
            axis.title.y.right = element_blank(),
            plot.title = element_text(size = 18),
            axis.title.y.left = element_text(size = 16),
            axis.title.x.bottom = element_text(size = 16),
            axis.text.x.bottom = element_text(size = 14),
            axis.text.y.left = element_text(size = 14),
            plot.caption = element_markdown(size = 14),
            axis.text.y.right = element_blank(),
            #panel.grid.major.y = element_line(),
            #panel.grid.minor.y = element_line(),
            legend.position = "bottom",
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 14)) +
          scale_color_brewer(palette="Dark2")
        
        #Export the graph to the scenario-specific folder in wd()
        png(filename = paste0(getwd(),sub('.', '', platform_defaults$roots$output),"/", time_stamp,"/", all_scenarios$scenario_names[eekeek],
                              "/chart_packs/",choices_i[k],".png"))
        print(graph)
        dev.off()
      }
    }
}
  

#----Some house cleaning to ensure no confusion with the scenario names - double-checked
  
all_scenarios <- as.data.frame(list.files(paste0(getwd(), sub('.', '', platform_defaults$roots$output),"/", time_stamp)))
colnames(all_scenarios) <- "scenario_names"

scenario_path_list <- list()
for (scenario in 1:nrow(all_scenarios)){
  scenario_path <- paste0(getwd(), sub('.', '', platform_defaults$roots$output),"/", time_stamp, "/", all_scenarios$scenario_names[scenario], "/")
  scenario_path_list <- append(scenario_path, scenario_path_list)

}
all_scenarios$file_path <- unlist(rev(scenario_path_list))

all_scenarios$name_dot <- all_scenarios$scenario_names %>%
  str_replace_all("-", ".")
#-----

#Step .08 - reshape the data from long to wide, export to specific
#   non-baseline scenario directory

  for(trial_time_02 in 1:length(long_dataframes_list)){
    to_export <- as.data.frame(long_dataframes_list[trial_time_02])
    number_of_position <- trial_time_02

    for (colum in 1:ncol(to_export)){
    colnames(to_export)[colum] <-  sub(".*_clean_long_df.", "", colnames(to_export)[colum])
    }
    
    to_export$year <- format(as.Date(to_export$year, format="%Y-%m-%d"),"%Y")
    

    for(cc in 1:length(all_scenarios)){
    if("scenario" %in% colnames(to_export)){
      int <- to_export %>%
        filter(grepl(paste0(all_scenarios$name_dot[cc],"_value"),variable)) %>%
        filter(grepl(paste0("delta"),variable)) %>%
        select(-c(variable)) %>%
        pivot_wider(names_from = scenario, values_from = value)


      write_csv(int, paste0(getwd(),sub('.', '', platform_defaults$roots$output),"/", time_stamp,"/", all_scenarios$scenario_names[cc],
                             "/","deltas/", all_csvs$csv_names[number_of_position], ".csv"))

      } else{
      write_csv(to_export, paste0(getwd(),sub('.', '', platform_defaults$roots$output),"/", time_stamp,"/", all_scenarios$scenario_names[cc],
                                  "/","deltas/", all_csvs$csv_names[number_of_position], ".csv"))
        }
      }
    }
}
