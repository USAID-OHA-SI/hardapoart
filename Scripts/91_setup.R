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
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  get_metadata() #list of MSD metadata elements

  load_secrets()

  #SID_Global_Dataset Final 2.0.xlsx
  gs_id <- as_sheets_id("1nn4c9NBsYchD6xUjimbWBB-4tHvrc4-AnWntGOk0XLc")

# IMPORT ------------------------------------------------------------------
  
  df_msd <- si_path() %>% 
    return_latest("OU_PSNU") %>% 
    read_msd()   
  
  df_fsd <- si_path() %>% 
    return_latest("Financial") %>% 
    read_msd()   
  
  si_path() %>% 
    return_latest("HRH") %>%
    unzip(exdir = folderpath_tmp)
  
  df_hrh <- folderpath_tmp %>% 
    return_latest("HRH.*xlsx") %>%
    read_excel(col_types = "text")

  df_sid <- range_speedread(gs_id,
                            col_types = c(
                              .default = "c",
                              SIDweighted_answer = "d",
                              SIDraw = "d"))
  
  df_unaids_tt <- pull_unaids(TRUE, "HIV Test & Treat")
  
  df_unaids_epi <- pull_unaids(FALSE, "epicontrol")

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
