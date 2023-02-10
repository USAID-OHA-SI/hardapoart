## 
## HOLD THIS FOR LATER
##

# Get historical periods
curr_pds <- metadata_msd$curr_fy %>% 
  paste0("Q", 1:metadata_msd$curr_qtr)

prev_pds <- (metadata_msd$curr_fy - 1) %>% 
  paste0("Q", 1:metadata_msd$curr_qtr)

hist_pds <- curr_pds %>% 
  c(prev_pds) %>% 
  sort(decreasing = T) %>% 
  magrittr::extract(1:5)

# Multiply Ind x historical Periods
df_msd_hist <- hist_pds %>% 
  tibble(period = .) %>% 
  cross_join(df_msd_ind) %>% 
  mutate(
    indicator = case_when(
      str_detect(indicator, "_D$") ~ str_remove(indicator, "_D$"),
      TRUE ~ indicator
    ),
    numdenom = case_when(
      str_detect(standardizeddisaggregate, "Total ") ~ str_remove(standardizeddisaggregate, "Total "),
      TRUE ~ "Numerator"
    ),
    standardizeddisaggregate = case_when(
      standardizeddisaggregate == "KeyPopAbr" ~ "KeyPop",
      TRUE ~ standardizeddisaggregate
    ))

# Query Data => 1 OU at the time
grabr::get_outable() %>% 
  distinct(operatingunit) %>% 
  pull(operatingunit) %>% 
  #first() %>% # Remove or move down the list with `nth()`
  nth(17) %>% 
  map_dfr(function(.ou){
    print(.ou)
    
    df_query <- df_msd_hist %>% 
      # Temp filter
      filter(period == paste0(metadata_msd$curr_fy, "Q", metadata_msd$curr_qtr),
             str_detect(indicator, "^TX"), 
             str_detect(standardizeddisaggregate, "^Total", negate = T)) %>%
      pmap_dfr(possibly(function(..1, ..2, ..3) {
        
        .pd = ..1
        .ind = ..2
        .sdg = ..3
        
        print(.pd)
        print(.ind)
        print(.sdg)
        
        if(str_detect(.sdg, "^Total")) {
          .disaggs <- NULL
        } else {
          .disaggs <- .sdg
        }
        
        if(str_detect(.sdg, "KeyPop")) {
          .dims <- c("Key Populations v3")
        } else {
          .dims <- c("Age: Cascade Age bands", "Sex", "Numerator / Denominator")
        }
        
        datim_query(ou = .ou, 
                    level = "prioritization",
                    pd = .pd,
                    ta = .ind,
                    disaggs = .disaggs,
                    dimensions = .dims)
      }, NULL))
    
    return(df_query)
  })