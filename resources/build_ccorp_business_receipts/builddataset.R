root = "resources/build_ccorp_business_receipts"



get_investment_data = function(y) {
  historical = file.path(root, "historical.csv") %>%
    fread() %>%
    tibble()
  
  historical = historical %>%
    filter(year == y) %>%
    pivot_longer(cols = !year, names_to = "detailed_industry", values_to = "investment") %>%
    mutate(
      detailed_industry = sub('.*\\.', '', detailed_industry)
    ) %>%
    group_by(detailed_industry) %>%
    summarise(
      investment = sum(investment)
    )
  return(historical)
}


#--------------------------------------------
# clean all coorperations data (crr table 1) 
#--------------------------------------------

root = "resources/build_ccorp_business_receipts/inputs"

allcorp_data = data.frame()
for(y in 2007:2021){
  allcorp_data = allcorp_data %>%
    bind_rows(
      # read in soi tax data
      file.path(root, paste0("allcorp", y, ".csv")) %>%
        fread() %>%
        tibble() %>%
        
        # compute receipts for merged `detailed_industry`s using shares of investment
        left_join(get_investment_data(y), by = "detailed_industry")  %>%
        group_by(soi_industry) %>% 
        mutate(
          weight = investment/sum(investment)
        ) %>%
        ungroup() %>%
        group_by(detailed_industry) %>%
        summarise(
          allcorp = as.double(sum(business_receipts*weight))
        ) %>%
        ungroup() %>%
        mutate(
          year = y
        )
    )
}


#------------------------------------------------
# clean c coorperations data (crr table 53 & 12) 
#------------------------------------------------

ccorp_data = data.frame()
for(y in 2007:2021){
  ccorp_data = ccorp_data %>%
    bind_rows(
      # read in c corp soi tax data
      file.path(root, paste0("ccorp", y, ".csv")) %>%
        fread() %>%
        tibble() %>%
        
        # compute receipts for missing `soi_industry`s using allcorp shares
        left_join(allcorp_data %>%
                    filter(year == y) %>%
                    select(!year),
                  by = "detailed_industry") %>%
        
        group_by(remainder) %>%
        mutate(
          weight = ifelse(is.na(business_receipts),
                          allcorp/sum(allcorp),
                          NA)
        ) %>%
        ungroup() %>%
        mutate(
          business_receipts = case_when(
            is.na(business_receipts) ~ remainder * weight,
            .default = business_receipts
          )
        ) %>%
        ungroup() %>%
        
        # compute receipts for merged `detailed_industry`s using allcorp shares
        group_by(soi_industry) %>%
        mutate(
          weight = allcorp/sum(allcorp)
        ) %>%
        ungroup() %>%
        group_by(detailed_industry) %>%
        summarise(
          ccorp = as.double(sum(business_receipts*weight))
        ) %>%
        ungroup() %>%
        mutate(
          year = y
        )
    )
}  


#---------------------------------------
# clean partnerships data (pa table 1) 
#---------------------------------------

# 2014 was last year to use major industries as opposed to industry groups
# the industry shares in this year are used for later years
pa14_weights = file.path(root, "pa2015.csv") %>%
  fread() %>%
  tibble() %>%
  left_join(
    file.path(root, "pa2014.csv") %>%
      fread() %>%
      tibble() %>%
      rename(
        br2014 = business_receipts,
        ind2014 = soi_industry
      ),
    by = "detailed_industry"
  ) %>%
  group_by(soi_industry) %>%
  mutate(
    weight = br2014/sum(unique(br2014))
  ) %>%
  ungroup() %>%
  select(!any_of(c("business_receipts","br2014")))



pa_data = data.frame()
for(y in 2007:2021){
  pa_data = pa_data %>%
    bind_rows(
      # read in pa soi tax data
      file.path(root, paste0("pa", y, ".csv")) %>%
        fread() %>%
        tibble() %>%
        
        # impute industries for 2015-2021 using 2014 shares
        {if (y>2014)
          distinct(., soi_industry, .keep_all=TRUE) %>%
            select(!detailed_industry) %>%
            left_join(pa14_weights, by = "soi_industry") %>%
            mutate(
              business_receipts = business_receipts * weight
            ) %>%
            select(detailed_industry, ind2014, business_receipts) %>%
            rename(soi_industry = ind2014)
          else .} %>%
        
        # compute receipts for merged `detailed_industry`s using allcorp shares
        left_join(allcorp_data %>%
                    filter(year == y) %>%
                    select(!year),
                  by = "detailed_industry") %>%
        group_by(soi_industry) %>%
        mutate(
          weight = allcorp/sum(allcorp)
        ) %>%
        ungroup() %>%
        group_by(detailed_industry) %>%
        summarise(
          pa = as.double(sum(business_receipts*weight))
        ) %>%
        ungroup() %>%
        mutate(
          year = y
        )
    )
}  








# business_receipts_data = allcorp_data %>%
#   left_join(
#     ccorp_data,
#     by = c("detailed_industry", "year")
#   ) %>%
#   left_join(
#     pa_data,
#     by = c("detailed_industry", "year")
#   ) %>%
#   mutate(
#     pct = ccorp/(allcorp + pa)
#   ) %>%
#   select(!any_of(c("allcorp","ccorp","pa"))) %>%
#   pivot_wider(
#     names_from = year,
#     values_from = pct
#   )
# write_csv(business_receipts_data, "meow2.csv")



#-----------------------------------------------------
# clean nonfarm sole propieterships data (sp table 1)
#-----------------------------------------------------

sp_data = data.frame()
for(y in 2007:2021){
  sp_data = sp_data %>%
    bind_rows(
      # read in sp soi tax data
      file.path(root, paste0("sp", y, ".csv")) %>%
        fread() %>%
        tibble() %>%
        
        # compute receipts for merged `detailed_industry`s using allcorp shares
        left_join(allcorp_data %>%
                    filter(year == y) %>%
                    select(!year),
                  by = "detailed_industry") %>%
        group_by(soi_industry) %>%
        mutate(
          weight = allcorp/sum(allcorp)
        ) %>%
        ungroup() %>%
        group_by(detailed_industry) %>%
        summarise(
          sp = as.double(sum(business_receipts*weight))
        ) %>%
        ungroup() %>%
        mutate(
          year = y
        )
    )
}  






#-----------------------------------------------------
# build
#-----------------------------------------------------


business_receipts_data = allcorp_data %>%
  left_join(
    ccorp_data,
    by = c("detailed_industry", "year")
  ) %>%
  left_join(
    pa_data,
    by = c("detailed_industry", "year")
  ) %>%
  left_join(
    sp_data,
    by = c("detailed_industry", "year")
  ) %>%
  mutate(
    pct = ccorp/(allcorp + pa + sp),
    pct = case_when(
      pct > 1 ~ 1,
      .default = pct
    )
  ) %>%
  select(!any_of(c("allcorp","ccorp","pa","sp"))) %>%
  pivot_wider(
    names_from = year,
    values_from = pct
  ) %>%
  rename(
    industry = detailed_industry
  ) %>%
  rev() %>%
  relocate(industry)

write_csv(business_receipts_data, paste0("resources/build_ccorp_business_receipts/ccorp_business_receipts.csv"))


# remaining c corp years
# sp weighted by allcorp
# pa weighted by 2014 pa

