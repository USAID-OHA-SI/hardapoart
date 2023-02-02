# PROJECT:  hardapoart
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  optimal mix of PEPFAR spending?
# REF ID:   e258e5d3 
# LICENSE:  MIT
# DATE:     2023-02-02
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
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "e258e5d3" #id for adorning to plots, making it easier to find on GH
  
  get_metadata() #list of MSD metadata elements

  load_secrets("email")
  
# IMPORT ------------------------------------------------------------------
  
  df_fsd <- si_path() %>% 
    return_latest("Fin") %>% 
    read_msd()   
  

# MUNGE -------------------------------------------------------------------

  #remove M&O and supply chain
  df_fsd <- df_fsd %>% 
    remove_mo() %>% 
    remove_sch()


  #assign funding type
    df_fsd %>% 
      mutate(funding_type = case_when(interaction_type %in% c("Not Specified", "Service Delivery") ~ interaction_type,
                                    program == "ASP" ~ "Above Site NSD",
                                    program %in% c("PM", "Applied Pipeline") ~ program,
                                    interaction_type == "Non Service Delivery" ~ "Site Level NSD")) 

