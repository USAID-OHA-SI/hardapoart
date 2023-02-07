# PROJECT:  hardapoart
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  to visualize linkage across psnus, gaps for priority pops
# REF ID:   f6f26589 
# LICENSE:  MIT
# DATE:     2023-02-07
# UPDATED: 

# DEPENDENCIES ----------------------------------------------------------------
  
  library(gagglr)
  library(tidyverse)
  library(scales)
  library(sf)
  library(glue)
  library(extrafont)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(selfdestructin5)
  library(gt)
  library(cascade) # Use dev version
  library(ggpattern)
  library(rcartocolor)

# GLOBAL VARIABLES ------------------------------------------------------------
  
  ref_id <- "f6f26589"
  ou <- "South Sudan"
  path <- "Genie_PSNU_IM_South_Sudan_Daily_2022-11-15"

# IMPORT ----------------------------------------------------------------------
  
  df <- si_path() %>%
    return_latest(path) %>%
    read_psd()
  
  load_secrets()
  get_metadata()

# MUNGE -----------------------------------------------------------------------
  
  df_msd <- df  %>% 
    filter(operatingunit == ou) %>%
    clean_agency() %>%
    resolve_knownissues()
  
  # Linkage Trends
  df_hts_base <- df_msd %>% 
    filter(indicator %in% c("HTS_TST_POS", "TX_NEW", "HTS_TST"), 
           standardizeddisaggregate == "Total Numerator",
           fiscal_year <= metadata$curr_fy, 
           funding_agency == "USAID") %>% 
    group_by(indicator, fiscal_year) %>% 
    summarise(across(targets:qtr4, \(x) sum(x, na.rm = T)), 
                     .groups = "drop") %>% 
    reshape_msd(direction ="semi-wide") %>% 
    group_by(indicator) %>% 
    fill(targets, .direction = "down") %>% 
    filter(nchar(period) != 4) 
  
  
  
  
  
  
  
  
  
  