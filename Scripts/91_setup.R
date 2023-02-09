# PROJECT:  hardapoart
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  import and setup data for markdown
# REF ID:   3fa8f096 
# LICENSE:  MIT
# DATE:     2023-02-06
# UPDATED:  2023-02-09

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
  library(mindthegap)
  library(sf)
  library(selfdestructin5)
  library(gt)
  library(cascade) # Use dev version
  library(ggpattern)
  library(rcartocolor)

# SOURCE LOCAL FUNCTIONS --------------------------------------------------

  #source("Scripts/11_sid_comp.R")
  source("Scripts/12_budget-trends_agency.R")
  source("Scripts/13_funding-distro_funding-flavors.R")
  source("Scripts/14_hrh-footprint-various.R")
  source("Scripts/16_hrh-titles.R")

# GLOBAL VARIABLES --------------------------------------------------------
  
  load_secrets("email")

  #SID_Global_Dataset Final 2.0.xlsx
  sid_gs_id <- as_sheets_id("1nn4c9NBsYchD6xUjimbWBB-4tHvrc4-AnWntGOk0XLc")
  
  #HIV Policy Lab data
  pol_lab_id <- as_sheets_id("1LadXX9g9D6MCp6M3WD27Js85YE8MHrf_ulJQmRVduCU")

# LOAD MSD ----------------------------------------------------------------

  #store meta data
  get_metadata(type = "PSNU_IM")
  metadata_msd <- metadata
  rm(metadata)
  
  #import
  df_msd <- si_path() %>% 
    return_latest("PSNU_IM") %>% 
    read_psd()   
  
  #resolve known issues
  df_msd <- resolve_knownissues(df_msd)
  
  #filter to data from last 5 quarters & relevant indicators/disaggs
  df_msd <- df_msd %>% 
    filter(fiscal_year >= 2022)
  
  
  #add _D to denom variables
  df_msd <- clean_indicator(df_msd)
  
  #MSD filter table
  df_msd_ind <- tibble::tribble(
    ~indicator,      ~standardizeddisaggregate,
    "HTS_TST",              "Total Numerator",
    "HTS_TST_POS",              "Total Numerator",
    "KP_PREV",              "Total Numerator",
    "OVC_SERV",              "Total Numerator",
    "PMTCT_EID",              "Total Numerator",
    "PrEP_NEW",                      "Age/Sex",
    "PrEP_NEW",                    "KeyPopAbr",
    "PrEP_NEW",              "Total Numerator",
    "TB_PREV",              "Total Numerator",
    "TX_CURR",            "Age/Sex/HIVStatus",
    "TX_CURR",              "Total Numerator",
    "TX_NEW",              "Total Numerator",
    "TX_PVLS", "Age/Sex/Indication/HIVStatus",
    "TX_PVLS",              "Total Numerator",
    "TX_PVLS_D",            "Total Denominator",
    "VMMC_CIRC",              "Total Numerator"
  )

  #filter to select indicators/disaggs
  df_msd <- df_msd %>% 
    semi_join(df_msd_ind, by = c("indicator", "standardizeddisaggregate"))
  
  

# LOAD FSD ----------------------------------------------------------------

  #store meta data
  get_metadata(type = "Financial")
  metadata_fsd <- metadata
  rm(metadata)
  
  #import
  df_fsd <- si_path() %>% 
    return_latest("Financial") %>% 
    read_psd()   
  
# LOAD NAT_SUBNAT ---------------------------------------------------------

  #store meta data
  get_metadata(type = "NAT_SUBNAT")
  metadata_natsubnat <- metadata
  rm(metadata)
  
  #import
  df_natsubnat <- si_path() %>% 
    return_latest("NAT_SUBNAT") %>% 
    read_psd()
  
  df_natsubnat_ind <- tibble::tribble(
    ~indicator, ~standardizeddisaggregate,
       "PLHIV",       "Age/Sex/HIVStatus",
     "POP_EST",                 "Age/Sex"
    )

  #filter to select indicators/disaggs
  df_natsubnat <- df_natsubnat %>% 
    semi_join(df_natsubnat_ind, by = c("indicator", "standardizeddisaggregate"))

# LOAD HRH ----------------------------------------------------------------

  #store meta data
  get_metadata(type = "HRH")
  metadata_hrh <- metadata
  rm(metadata)
  
  #import
  df_hrh <- si_path() %>% 
    return_latest("HRH") %>% 
    read_psd()
  
  #limit to latest fy
  df_hrh <- df_hrh %>% 
    filter(fiscal_year == max(fiscal_year, na.rm = TRUE))

# LOAD SID ----------------------------------------------------------------

  #store meta data
  metadata_sid <- list(caption = "Source: FY21 SID Global Dataset")
  
  #import
  df_sid <- range_speedread(sid_gs_id,
                            col_types = c(
                              .default = "c",
                              SIDweighted_answer = "d",
                              SIDraw = "d"))
  
  #filter to max year
  df_sid <- filter(df_sid, fiscal_year == max(fiscal_year))
  

# LOAD UNAIDS -------------------------------------------------------------


  #store meta data
  metadata_unaids <- list(caption = glue("Source: {source_note()}"))
  
  #import
  df_unaids_tt <- pull_unaids(TRUE, "HIV Test & Treat")
  
  df_unaids_epi <- pull_unaids(FALSE, "epicontrol")
  
# LOAD 10-10-10 ------------------------------------------------------------
  
  #store meta data
  metadata_tens <- list(caption = "Source: HIV Policy Lab [2021-11-09]")
  
  #read in HIV Policy Lab data export
  df_tens <- googlesheets4::range_speedread(pol_lab_id, "Policy adoption data", skip = 6,
                                            col_types = "c") %>% 
    janitor::clean_names()
  

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
          input = here("Scripts","reports.Qmd"))
