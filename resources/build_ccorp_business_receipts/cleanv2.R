root = "resources/build_ccorp_business_receipts"

suppressPackageStartupMessages(
  invisible(capture.output(
    lapply(readLines('resources/build_ccorp_business_receipts/requirements.txt'), library, character.only = T)    
  ))
)
library(readxl)


sp_inputs = tibble(
  year = NA,
  file = NA
)
pa_inputs = tibble(
  year = NA,
  file = NA
)
allcorp_inputs = tibble(
  year = NA,
  file = NA
)
ccorp_inputs = tibble(
  year = NA,
  file = NA
)

for(y in 2004:2021){
  sp_inputs = sp_inputs %>%
    bind_rows(
      tibble(
        year = y,
        file = paste0(substr(y,3,4), 'sp01br.xls')
      )
    ) %>% drop_na()
}

for (y in 2014:2021){
  allcorp_inputs = allcorp_inputs %>%
    bind_rows(
      tibble(
        year = y,
        file = paste0(substr(y,3,4), 'co01ccr.xlsx')
      )
    ) %>% drop_na()
  ccorp_inputs = ccorp_inputs %>%
    bind_rows(
      tibble(
        year = y,
        file = paste0(substr(y,3,4), 'co53ccr.xlsx')
      )
    ) %>% drop_na()
}
for(y in 2004:2013){
  allcorp_inputs = allcorp_inputs %>%
    bind_rows(
      tibble(
        year = y,
        # manually renamed 2004 and 2005
        file = paste0(substr(y,3,4), 'co01ccr.xls')
      )
    ) %>% drop_na()
}
for(y in 2004:2013){
  ccorp_inputs = ccorp_inputs %>%
    bind_rows(
      tibble(
        year = y,
        file = paste0(substr(y,3,4), 'co12ccr.xls')
      )
    ) %>% drop_na()
  
}


for(y in 2015:2021){
  pa_inputs = pa_inputs %>%
    bind_rows(
      tibble(
        year = y,
        file = paste0(substr(y,3,4), 'pa01.xlsx')
      )
    ) %>% drop_na()
}
# start in 04
for(y in 2004:2014){
  pa_inputs = pa_inputs %>%
    bind_rows(
      tibble(
        year = y,
        file = paste0(substr(y,3,4), 'pa01.xls')
      )
    ) %>% drop_na()
}

sp_inputs = sp_inputs %>%
  left_join(
    file.path(root, "crosswalks/cw.csv") %>%
      fread() %>%
      tibble() %>%
      filter(type == "sp") %>%
      select(!type),
    by = "year"
  )
pa_inputs = pa_inputs %>%
  left_join(
    file.path(root, "crosswalks/cw.csv") %>%
      fread() %>%
      tibble() %>%
      filter(type == "pa") %>%
      select(!type),
    by = "year"
  )

allcorp_inputs = allcorp_inputs %>%
  left_join(
    file.path(root, "crosswalks/cw.csv") %>%
      fread() %>%
      tibble() %>%
      filter(type == "allcorp") %>%
      select(!type),
    by = "year"
  )

ccorp_inputs = ccorp_inputs %>%
  left_join(
    file.path(root, "crosswalks/cw.csv") %>%
      fread() %>%
      tibble() %>%
      filter(type == "ccorp") %>%
      select(!type),
    by = "year"
  )





fl = function(y,str) {
  if (str == "allcorp"){
    return(allcorp_inputs[allcorp_inputs$year== y,]$file)
  } else if (str == "pa"){
    return(pa_inputs[pa_inputs$year== y,]$file)
  } else if (str == "sp"){
    return(sp_inputs[sp_inputs$year== y,]$file)
  } else if (str == "ccorp"){
    return(ccorp_inputs[ccorp_inputs$year== y,]$file)
  } else{
    return("meow")
  }
}

cw_fl = function(y,str) {
  if (str == "allcorp"){
    return(allcorp_inputs[allcorp_inputs$year== y,]$cw_file)
  } else if (str == "pa"){
    return(pa_inputs[pa_inputs$year== y,]$cw_file)
  } else if (str == "sp"){
    return(sp_inputs[sp_inputs$year== y,]$cw_file)
  } else if (str == "ccorp"){
    return(ccorp_inputs[ccorp_inputs$year== y,]$cw_file)
  } else{
    return("meow")
  }
}


write_allcorp = function(year, file, cw_file) {
  print(file)
  allcorp = suppressMessages(read_excel(paste0(root, "/data/", file))) %>% tibble()
  
  
  if(!grepl('usiness', allcorp[6])){
    print(head(allcorp[,6], 15))
    stop("not business  receipts data")
  }
  
  allcorp = allcorp %>%
    select(1,6) %>%
    rename(
      industry = 1,
      business_receipts = 2
    ) %>%
    drop_na() %>%
    mutate(
      business_receipts = suppressWarnings(as.numeric(business_receipts))
    )
  
  if (!(nrow(allcorp) %in% c(247,252))){
    stop(paste0(file, " - expected 247 but ", nrow(allcorp)))
  }
  
  if (year <= 2014){
    allcorp = allcorp %>%
      mutate(
        industry = gsub("\\s+", " ", str_trim(industry))
      )
  }
  
  cw = file.path(root, "crosswalks", cw_file) %>%
    fread() %>%
    tibble()
  
  meow = allcorp %>%
    right_join(cw, by = "industry")
  
  if(!is.na(which(is.na(meow$business_receipts))[1])){
    print(meow[which(is.na(meow$business_receipts)),])
    view(allcorp)
    stop(paste0(file, " is missing data"))
  }
  
  meow = meow %>%
    rename(
      soi_industry = industry
    ) %>% relocate(detailed_industry)
  
  write_csv(meow, paste0(root, "/inputs/allcorp",year,".csv"))
}
write_sp = function(year, file, cw_file) {
  print(file)
  
  sp = suppressMessages(read_excel(paste0(root, "/data/", file))) %>% tibble()
  
  if(!(grepl('usiness', sp[3]) & grepl('eceipt',sp[3]))){
    print(head(sp[,3], 15))
    stop("not business  receipts data")
  }
  
  sp = sp %>%
    select(1,3) %>%
    rename(
      industry = 1,
      business_receipts = 2
    ) %>%
    mutate(
      industry = gsub("\\s+", " ", str_trim(industry)),
      business_receipts = suppressWarnings(as.numeric(business_receipts))
    ) %>%
    drop_na()
  
  if (!(nrow(sp) %in% c(155,148,147,159))){
    stop(paste0(file, " - expected 155... and others but ", nrow(sp)))
  }
  
  cw = file.path(root, "crosswalks", cw_file) %>%
    fread() %>%
    tibble()
  meow = sp %>%
    right_join(cw, by = "industry")
  
  if(!is.na(which(is.na(meow$business_receipts))[1])){
    print(meow[which(is.na(meow$business_receipts)),])
    view(sp)
    stop(paste0(file, " is missing data"))
  }
  
  meow = meow %>%
    rename(
      soi_industry = industry
    ) %>% relocate(detailed_industry) %>%
    mutate(
      business_receipts = case_when(
        grepl("Farms|Insurance_Carriers",detailed_industry) ~ 0,
        .default = business_receipts 
      )
    )
  write_csv(meow, paste0(root, "/inputs/sp",year,".csv"))
  return(meow)
}
write_pa = function(year, file, cw_file) {
  print(file)
  
  if(year >= 2015){
    pa = suppressMessages(read_excel(paste0(root, "/data/", file))) %>%
      tibble() %>%
      drop_na()
    
    # fuck strings man ...  
    colnames(pa) = gsub('\\[[[:digit:]]+\\]', '', pa[pa[1]=="Item",])
    colnames(pa) = gsub("\\s+", " ", str_trim(str_replace_all(colnames(pa), "[:space:]", " ")))
    pa = pa[, !duplicated(colnames(pa))] %>%
      filter(.[[1]] == "Business receipts") %>%
      select(!1) %>%
      pivot_longer(
        cols = everything(), names_to = "industry", values_to = "business_receipts"
      ) %>% mutate(
        business_receipts = as.numeric(business_receipts)
      )
    
    if (nrow(pa) != 20){
      stop(paste0(file, " - expected 20 but ", nrow(pa)))
    }
  } else{
    pa = suppressMessages(read_excel(paste0(root, "/data/", file)))
    
    for(i in 2:ncol(pa)){
      if(is.na(pa[which(pa[1] == "Item")[1],i])){
        if(is.na(pa[which(pa[1] == "Item")[1]+1,i])){
          pa[which(pa[1] == "Item")[1],i] = pa[which(pa[1] == "Item")[1]+2,i]
        } else{
          pa[which(pa[1] == "Item")[1],i] = pa[which(pa[1] == "Item")[1]+1,i]
        }
      }
    }
    colnames(pa) = gsub("\\s+", " ", str_trim(str_replace_all(pa[which(pa[1] == "Item")[1],], "[:space:]", " ")))
    
    
    # fuck strings man ...  
    pa = pa[, !duplicated(colnames(pa))] %>%
      filter(.[[1]] == "Business receipts") %>%
      select(!1) %>%
      pivot_longer(
        cols = everything(), names_to = "industry", values_to = "business_receipts"
      ) %>% mutate(
        business_receipts = as.numeric(business_receipts)
      )
    
    if (!(nrow(pa) %in% c(136,137))){
      stop(paste0(file, " - expected 136 but ", nrow(pa)))
    }
  }
  
  cw = file.path(root, "crosswalks", cw_file) %>%
    fread() %>%
    tibble()
  
  meow = pa %>%
    right_join(cw, by="industry")
  
  if(!is.na(which(is.na(meow$business_receipts))[1])){
    print(meow[which(is.na(meow$business_receipts)),])
    view(pa)
    stop(paste0(file, " is missing data"))
  }
  
  meow = meow %>%
    rename(
      soi_industry = industry
    ) %>% relocate(detailed_industry)
    
  write_csv(meow, paste0(root, "/inputs/pa",year,".csv"))
  
  # meow = meow %>%
  #   left_join(get_investment(year), by = "detailed_industry") %>%
  #   group_by(`industry`) %>% 
  #   mutate(
  #     weight = investment/sum(investment)
  #   ) %>%
  #   group_by(detailed_industry) %>%
  #   summarise(
  #     
  #     pa = sum(business_receipts*weight)
  #   )
  # return(meow)
}
write_ccorp = function(year, file, cw_file) {
  
  print(file)
  
  ccorp = suppressMessages(read_excel(paste0(root, "/data/", file)))
  
  if(year >= 2013) {
    group = "group"
    ccorp[which(ccorp[1] == "Item")[1]-1,1] = group
    for(i in 2:ncol(ccorp)){
      if(is.na(ccorp[which(ccorp[1] == "Item")[1],i])){
        j=1
        while(is.na(ccorp[which(ccorp[1] == "Item")[1]+j,i])){
          j=j+1
        }
        ccorp[which(ccorp[1] == "Item")[1],i] = ccorp[which(ccorp[1] == "Item")[1]+j,i]
      } else {
        group = ccorp[which(ccorp[1] == "Item")[1],i]
      }
      ccorp[which(ccorp[1] == "Item")[1]-1,i] = group
    }
    colnames(ccorp) = gsub("\\s+", " ", str_trim(str_replace_all(ccorp[which(ccorp[1] == "Item")[1],], "[:space:]", " ")))
    # ccorp[which(ccorp[1] == "Total receipts")[1],1] = "totalrec"
  } else {
    if(length(which(!is.na(ccorp[1,])))>1){
      ccorp = ccorp[,-which(!is.na(ccorp[1,]))[-1]]
    }
    ccorp[1,1] = "group"
    ccorp[2,1] = "industry"
    group = ""
    for(i in 2:ncol(ccorp)){
      j=1
      industry = ""
      while(!grepl("[[:digit:]]", ccorp[j,i])){
        if(!is.na(ccorp[j,i])){
          industry = paste(industry, ccorp[j,i], sep = " ")
        }
        j = j+1
      }
      industry = gsub(".*continued","",industry)
      if(grepl("Total", industry)){
        industry = str_replace(industry, "Total", "")
        group = industry
      }
      ccorp[1,i] = group
      ccorp[2,i] = industry
    }
    colnames(ccorp) = gsub("\\s+", " ", str_trim(str_replace_all(ccorp[2,], "[:space:]", " ")))
    ccorp[1,] = as.list(gsub("\\s+", " ", str_trim(str_replace_all(ccorp[1,], "[:space:]", " "))))
    # ccorp[which(ccorp[1] == "Total receipts")[1],1] = "totalrec"
  }
  
  ccorp = ccorp %>%
    filter(grepl("Business receipts|Total receipts|group", .[[1]])) %>%
    select(!(1|2))
  meow = ccorp[1,] %>%
    pivot_longer(
      cols = everything(), names_to = "industry", values_to = "group"
    ) %>%
    left_join(
      ccorp[3,] %>%
        pivot_longer(
          cols = everything(), names_to = "industry", values_to = "business_receipts"
        ),
      by = "industry"
    ) %>%
    left_join(
      ccorp[2,] %>%
        pivot_longer(
          cols = everything(), names_to = "industry", values_to = "total_receipts"
        ),
      by = "industry"
    )
  
  ccorp = meow %>%
    mutate(
      not_dd = ifelse(business_receipts == "d",
                         ifelse(total_receipts == "d",
                                0,
                                1),
                         NA),
      business_receipts = suppressWarnings(as.numeric(business_receipts)),
      total_receipts = suppressWarnings(as.numeric(total_receipts))
    )
  ccorp$total = NA
  for(i in 1:nrow(ccorp)) {
    if(ccorp$group[i] == ccorp$industry[i]){
      total = ccorp$business_receipts[i]
      ccorp$group[i] = paste0(ccorp$group[i], ".topline")
    }
    ccorp$total[i] = total
  }
  ccorp = ccorp %>%
    group_by(group) %>%
    mutate(
      remainder = ifelse(!is.na(not_dd),
                         total-sum(business_receipts, na.rm=T),
                         NA),
      use_total_receipts = ifelse(!is.na(not_dd),
                         (sum(not_dd,na.rm=T) == sum(!is.na(not_dd))),
                         NA)
    ) %>%
    group_by(remainder) %>%
    mutate(
      weight = ifelse(use_total_receipts,
                      total_receipts/sum(total_receipts),
                      NA)
    ) %>%
    ungroup() %>%
    mutate(
      business_receipts = case_when(
        is.na(business_receipts) & !is.na(weight) ~ remainder*weight,
        .default = business_receipts
      )
        # ifelse(is.na(business_receipts) & !is.na(weight),
        #                          remainder*weight,
        #                          business_receipts)
    ) %>%
    select(!any_of(c("total_receipts","not_dd","total", "group")))
  
  #------------------------------------  
  # merge in investment data
  # group_by is_dd
  # calc weights
  # mutate business_receipts = business_receipts + weights*remainder
  # then do as above i.e. group by actual industry then calc weights then
  
  if (!(nrow(ccorp) %in% c(208,94))){
    stop(paste0(file, " - expected 208 or 94 but ", nrow(ccorp)))
  }
  
  cw = file.path(root, "crosswalks", cw_file) %>%
    fread() %>%
    tibble()
  
  meow = ccorp %>%
    right_join(cw, by="industry")
  
  if(!is.na(which(is.na(meow$business_receipts)&is.na(meow$remainder))[1])){
    print(meow[which(is.na(meow$business_receipts)),])
    view(ccorp)
    stop(paste0(file, " is missing data"))
  }
  
  meow = meow %>%
    rename(
      soi_industry = industry
    ) %>% relocate(detailed_industry)
  
  write_csv(meow, paste0(root, "/inputs/ccorp",year,".csv"))
    
  # meow = meow %>%
  #   left_join(get_investment(year), by = "detailed_industry") %>%
  #   group_by(remainder) %>%
  #   mutate(
  #     dweight = ifelse(is_dd == "",
  #                      0,
  #                      investment/sum(investment))
  #   ) %>%
  #   mutate(
  #     business_receipts = business_receipts + remainder * dweight
  #   ) %>%
  #   group_by(`industry`) %>% 
  #   mutate(
  #     weight = investment/sum(investment)
  #   ) %>%
  #   group_by(detailed_industry) %>%
  #   summarise(
  #     
  #     ccorp = sum(business_receipts*weight)
  #   )
  # return(meow)
}

str = list("sp","pa","allcorp","ccorp")
for(y in 2007:2021){
  for(s in str){
    file = fl(y,s)
    cw_file = cw_fl(y,s)
    print( paste0("write_", s, "(y,file,cw_file)"))
    eval(parse(text = paste0("write_", s, "(y,file,cw_file)")))
  }
}

# y=2016
# year = y
# str = "ccorp"
# file = fl(y,str)
# cw_file = cw_fl(y,str)
# meow = write_ccorp(y, file, cw_file)
