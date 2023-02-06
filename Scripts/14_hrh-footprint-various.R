# PROJECT:  hardapoart
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  hrh footprint
# REF ID:   d98b536f 
# LICENSE:  MIT
# DATE:     2023-02-06
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(gagglr)
  library(glue)
  library(scales)
  library(extrafont)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(vroom)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "d98b536f" #id for adorning to plots, making it easier to find on GH
  
  # get_metadata(type = "HRH") #list of MSD metadata elements

  cntry <- "Malawi"
  
# IMPORT ------------------------------------------------------------------
  
  df_hrh <- si_path() %>% 
    return_latest("HRH") %>% 
    vroom(col_types = list(
      fiscal_year = "i",
      annual_expenditure  = "d",
      annual_fringe = "d",
      actual_annual_spend = "d",
      equivalent_annual_spend = "d",
      equivalent_annual_salary = "d",
      avg_fte_per_month = "d",
      months_of_work = "d",
      individual_count = "i",
      annual_fte = "d",
      .default = "c"
    ))
  

# MUNGE -------------------------------------------------------------------
  
  #filter to select country
  df_hrh <- df_hrh %>% 
    filter(country == cntry)

  #assign funding type
  df_hrh <- df_hrh %>% 
    mutate(funding_type = case_when(interaction_type == "Service Delivery" ~ "Service Delivery (SD)",
                                    interaction_type == "Not Specified" ~ interaction_type,
                                    program == "ASP" ~ "Above Site Non-SD",
                                    program %in% c("PM", "Applied Pipeline") ~ program,
                                    interaction_type == "Non Service Delivery" ~ "Site Level Non-SD")) 
  
  #total staff count
  df_hrh %>% 
    group_by(fiscal_year) %>% 
    summarise(across(c(individual_count, annual_fte, actual_annual_spend),
                     \(x) sum(x, na.rm = TRUE)),
              .groups = "drop")
  
  
  #staff count by SD vs NSD
  df_hrh %>% 
    group_by(fiscal_year, funding_type) %>% 
    summarise(across(c(individual_count, annual_fte, actual_annual_spend),
                     \(x) sum(x, na.rm = TRUE)),
              .groups = "drop")
  
  #staff count by work location 
  df_hrh %>% 
    group_by(fiscal_year, work_location) %>% 
    summarise(across(c(individual_count, annual_fte, actual_annual_spend),
                     \(x) sum(x, na.rm = TRUE)),
              .groups = "drop") %>% 
    filter(work_location != "Validation Error")
  
  
  #staff count by work location 
  df_hrh %>% 
    group_by(fiscal_year, er_category) %>% 
    summarise(across(c(individual_count, annual_fte, actual_annual_spend),
                     \(x) sum(x, na.rm = TRUE)),
              .groups = "drop")
  
  #top staff by title
  df_hrh %>% 
    group_by(fiscal_year, employment_title) %>% 
    summarise(across(c(individual_count, annual_fte, actual_annual_spend),
                     \(x) sum(x, na.rm = TRUE)),
              .groups = "drop") %>% 
    slice_max(order_by = individual_count, n = 10)
    arrange(desc(individual_count))
