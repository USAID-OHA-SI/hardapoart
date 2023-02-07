# PROJECT:  hardapoart
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  import and setup data for markdown
# REF ID:   3fa8f096 
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
  library(googlesheets4)

# GLOBAL VARIABLES --------------------------------------------------------
  
  load_secrets()

  #SID_Global_Dataset Final 2.0.xlsx
  gs_id <- as_sheets_id("1nn4c9NBsYchD6xUjimbWBB-4tHvrc4-AnWntGOk0XLc")

# IMPORT ------------------------------------------------------------------

# LOAD MSD ----------------------------------------------------------------

  #store meta data
  get_metadata(type = "PSNU_IM")
  metadata_msd <- metadata
  rm(metadata)
  
  #import
  df_msd <- si_path() %>% 
    return_latest("PSNU_IM") %>% 
    read_psd()   
  
  #filter to data from last 5 quarters & relevant indicators/disaggs
  df_msd %>% 
    filter(fiscal_year >= 2022)
  
  
  
  
  
  
  
  df_fsd <- si_path() %>% 
    return_latest("Financial") %>% 
    read_psd() 
  
  df_natsubnat <- si_path() %>% 
    return_latest("NAT_SUBNAT") %>% 
    read_psd()
  
  df_hrh <- si_path() %>% 
    return_latest("HRH") %>% 
    read_psd()
  
  df_sid <- range_speedread(gs_id,
                            col_types = c(
                              .default = "c",
                              SIDweighted_answer = "d",
                              SIDraw = "d"))
  
  df_unaids_tt <- pull_unaids(TRUE, "HIV Test & Treat")
  
  df_unaids_epi <- pull_unaids(FALSE, "epicontrol")

# STORE METDATA -----------------------------------------------------------


  
  get_metadata("Financial")
  metadata_fsd <- metadata
  
  get_metadata("NAT_SUBNAT")
  metadata_natsubnat <- metadata

  get_metadata("HRH")
  metadata_hrh <- metadata  
  
  metadata_sid <- list(caption = "Source: FY21 SID Global Dataset")
  
  metadata_unaids <- list(caption = glue("Source: {mindthegap::source_note()}"))

# MUNGE -------------------------------------------------------------------
  
  #TODO
  

# MARKDOWN ----------------------------------------------------------------

  #TODO
  vct_cntry <- pepfar_country_list %>% 
    pull(country)
  
  #output files
  reports <- tibble(
    output_file = glue(here("markdown","{curr_pd}_{vct_cntry}_cop-support-viz_oha-siei.pptx")),
    params = map(vct_cntry, ~list(vct_cntry = .))
  )
  
  #create reports
  reports %>%
    pwalk(render, 
          input = here("Scripts","reports.Rmd"))
