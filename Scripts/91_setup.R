# PROJECT:  hardapoart
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  import and setup data for markdown
# REF ID:   3fa8f096 
# LICENSE:  MIT
# DATE:     2023-02-06
# UPDATED:  2023-02-13

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
  library(ggrepel)
  library(janitor)
  library(cascade) # Use dev version
  library(here)
  library(assertr)

# GLOBAL VARIABLES --------------------------------------------------------
  
  load_secrets("email")

  #SID_Global_Dataset Final 2.0.xlsx
  sid_gs_id <- "1nn4c9NBsYchD6xUjimbWBB-4tHvrc4-AnWntGOk0XLc"

  #HIV Policy Lab data
  pol_lab_id <- "1LadXX9g9D6MCp6M3WD27Js85YE8MHrf_ulJQmRVduCU"

# LOAD MSD ----------------------------------------------------------------


  if(!file.exists(return_latest(si_path(), "PSNU_IM"))){
    df_msd <- tibble::tibble(fiscal_year = NA,
                             country = NA,
                             fundingagency = NA)
  } else {

    #store meta data
    get_metadata(type = "PSNU_IM")
    metadata_msd <- metadata
    rm(metadata)

    #import
    df_msd <- si_path() %>%
      return_latest("PSNU_IM") %>%
      read_psd()

    #add _D to denom variables
    df_msd <- clean_indicator(df_msd)

    #MSD filter table
    df_msd_ind <-
      tibble::tribble(
               ~indicator,        ~standardizeddisaggregate,
            "AGYW_PREV_D",              "Total Denominator",
                "HTS_TST",                "Total Numerator",
                "HTS_TST",        "Modality/Age/Sex/Result",
            "HTS_TST_POS",                "Total Numerator",
                "KP_PREV",                "Total Numerator",
               "OVC_SERV",                "Total Numerator",
               "OVC_SERV",             "Age/Sex/Preventive",
               "OVC_SERV",          "Age/Sex/ProgramStatus",
               "OVC_SERV", "Age/Sex/ProgramStatusCaregiver",
            "OVC_HIVSTAT",                "Total Numerator",
        "OVC_HIVSTAT_POS",         "Age/Sex/ReportedStatus",
              "PMTCT_EID",                "Total Numerator",
               "PrEP_NEW",                        "Age/Sex",
               "PrEP_NEW",                      "KeyPopAbr",
               "PrEP_NEW",                "Total Numerator",
                "TB_PREV",                "Total Numerator",
                "TB_STAT",         "Age/Sex/KnownNewPosNeg",
              "TB_STAT_D",                        "Age/Sex",
                "TX_CURR",              "Age/Sex/HIVStatus",
                "TX_CURR",               "KeyPop/HIVStatus",
                "TX_CURR",                "Total Numerator",
                 "TX_NEW",                "Total Numerator",
                 "TX_NEW",              "Age/Sex/HIVStatus",
                "TX_PVLS",   "Age/Sex/Indication/HIVStatus",
                "TX_PVLS",    "KeyPop/Indication/HIVStatus",
                "TX_PVLS",                "Total Numerator",
              "TX_PVLS_D",   "Age/Sex/Indication/HIVStatus",
              "TX_PVLS_D",    "KeyPop/Indication/HIVStatus",
              "TX_PVLS_D",              "Total Denominator",
              "VMMC_CIRC",                "Total Numerator"
        )





    #filter to select indicators/disaggs
    df_msd <- df_msd %>%
      semi_join(df_msd_ind, by = c("indicator", "standardizeddisaggregate"))
    
      rm(df_msd_ind)
      
    #add in PEPFAR "agency"
    df_msd <- df_msd %>% 
      bind_rows(df_msd %>% mutate(funding_agency = "PEPFAR"))
    
    #clean agency name
    df_msd <- clean_agency(df_msd)
    
    #clean PSNU names
    df_msd <- clean_psnu(df_msd)
    
    #resolve known issues
    df_msd <- resolve_knownissues(df_msd)
    
  }

  # #resolve known issues
  # df_msd <- resolve_knownissues(df_msd)
  # 
  # #filter to data from last 5 quarters & relevant indicators/disaggs
  # df_msd <- df_msd %>%
  #   filter(fiscal_year >= 2022)
  # 
  # curr_pds <- metadata_msd$curr_fy %>%
  #   paste0("Q", 1:metadata_msd$curr_qtr)
  # 
  # prev_pds <- (metadata_msd$curr_fy - 1) %>%
  #   paste0("Q", 1:metadata_msd$curr_qtr)
  # 
  # hist_pds <- curr_pds %>%
  #   c(prev_pds) %>%
  #   sort(decreasing = T) %>%
  #   magrittr::extract(1:5)
  # 
  # 
  # #add _D to denom variables
  # df_msd <- clean_indicator(df_msd)
  # 
  # #MSD filter table
  # df_msd_ind <- tibble::tribble(
  #   ~indicator,      ~standardizeddisaggregate,
  #   "HTS_TST",              "Total Numerator",
  #   "HTS_TST_POS",              "Total Numerator",
  #   "KP_PREV",              "Total Numerator",
  #   "OVC_SERV",              "Total Numerator",
  #   "PMTCT_EID",              "Total Numerator",
  #   "PrEP_NEW",                      "Age/Sex",
  #   "PrEP_NEW",                    "KeyPopAbr",
  #   "PrEP_NEW",              "Total Numerator",
  #   "TB_PREV",              "Total Numerator",
  #   "TX_CURR",            "Age/Sex/HIVStatus",
  #   "TX_CURR",              "Total Numerator",
  #   "TX_NEW",              "Total Numerator",
  #   "TX_PVLS", "Age/Sex/Indication/HIVStatus",
  #   "TX_PVLS",              "Total Numerator",
  #   "TX_PVLS_D",            "Total Denominator",
  #   "VMMC_CIRC",              "Total Numerator"
  # )
  # 
  # #filter to select indicators/disaggs
  # df_msd <- df_msd %>%
  #   semi_join(df_msd_ind, by = c("indicator", "standardizeddisaggregate"))
  # 
  # # Multiply Ind x historical Periods
  # df_msd_hist <- hist_pds %>%
  #   tibble(period = .) %>%
  #   cross_join(df_msd_ind) %>%
  #   mutate(
  #     indicator = case_when(
  #       str_detect(indicator, "_D$") ~ str_remove(indicator, "_D$"),
  #       TRUE ~ indicator
  #     ),
  #     numdenom = case_when(
  #       str_detect(standardizeddisaggregate, "Total ") ~ str_remove(standardizeddisaggregate, "Total "),
  #       TRUE ~ "Numerator"
  #     ),
  #     standardizeddisaggregate = case_when(
  #       standardizeddisaggregate == "KeyPopAbr" ~ "KeyPop",
  #       TRUE ~ standardizeddisaggregate
  #     ))
  # 
  # # Query Data => 1 OU at the time
  # grabr::get_outable() %>%
  #   distinct(operatingunit) %>%
  #   pull(operatingunit) %>%
  #   #first() %>% # Remove or move down the list with `nth()`
  #   nth(17) %>%
  #   map_dfr(function(.ou){
  #     print(.ou)
  # 
  #     df_query <- df_msd_hist %>%
  #       # Temp filter
  #       filter(period == paste0(metadata_msd$curr_fy, "Q", metadata_msd$curr_qtr),
  #              str_detect(indicator, "^TX"),
  #              str_detect(standardizeddisaggregate, "^Total", negate = T)) %>%
  #       pmap_dfr(possibly(function(..1, ..2, ..3) {
  # 
  #         .pd = ..1
  #         .ind = ..2
  #         .sdg = ..3
  # 
  #         print(.pd)
  #         print(.ind)
  #         print(.sdg)
  # 
  #         if(str_detect(.sdg, "^Total")) {
  #           .disaggs <- NULL
  #         } else {
  #           .disaggs <- .sdg
  #         }
  # 
  #         if(str_detect(.sdg, "KeyPop")) {
  #           .dims <- c("Key Populations v3")
  #         } else {
  #           .dims <- c("Age: Cascade Age bands", "Sex", "Numerator / Denominator")
  #         }
  # 
  #         datim_query(ou = .ou,
  #                     level = "prioritization",
  #                     pd = .pd,
  #                     ta = .ind,
  #                     disaggs = .disaggs,
  #                     dimensions = .dims)
  #       }, NULL))
  # 
  #     return(df_query)
  #   })

# LOAD FSD ----------------------------------------------------------------

  if(!file.exists(return_latest(si_path(), "Financial"))){
    df_fsd <- tibble::tibble(fiscal_year = NA,
                             country = NA,
                             fundingagency = NA)
  } else {

    #store meta data
    get_metadata(type = "Financial")
    metadata_fsd <- metadata
    rm(metadata)

    #import
    df_fsd <- si_path() %>%
      return_latest("Financial") %>%
      read_psd()
    
    #clean agency name
    df_fsd <- clean_agency(df_fsd)
    
    #remove M&O and supply chain
    df_fsd <- df_fsd %>% 
      remove_mo() %>% 
      remove_sch()

  }

# LOAD NAT_SUBNAT ---------------------------------------------------------

  if(!file.exists(return_latest(si_path(), "NAT_SUBNAT"))){
    df_natsubnat <- tibble::tibble(fiscal_year = NA,
                             country = NA)
  } else {
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
          "DIAGNOSED_SUBNAT",       "Age/Sex/HIVStatus",
                     "PLHIV",       "Age/Sex/HIVStatus",
                   "POP_EST",                 "Age/Sex",
            "TX_CURR_SUBNAT",       "Age/Sex/HIVStatus",
     "VL_SUPPRESSION_SUBNAT",       "Age/Sex/HIVStatus"
     )


  #filter to select indicators/disaggs
  df_natsubnat <- df_natsubnat %>%
    semi_join(df_natsubnat_ind, by = c("indicator", "standardizeddisaggregate"))
  
  #clean PSNU names
  df_natsubnat <- clean_psnu(df_natsubnat)
  }


# LOAD HRH ----------------------------------------------------------------

  if(!file.exists(return_latest(si_path(), "HRH"))){
    df_hrh <- tibble::tibble(fiscal_year = NA,
                             country = NA,
                             fundingagency = NA)
  } else {
  #store meta data
  get_metadata(type = "HRH")
  metadata_hrh <- metadata
  rm(metadata)

  #import
  df_hrh <- si_path() %>%
    return_latest("HRH") %>%
    read_psd()

  #clean agency name
  df_hrh <- clean_agency(df_hrh)
  
  #limit to latest fy
  df_hrh <- df_hrh %>%
    filter(fiscal_year == max(fiscal_year, na.rm = TRUE))
  }

# LOAD SID ----------------------------------------------------------------

  #import
  df_sid <- range_speedread(as_sheets_id(sid_gs_id),
                            col_types = c(
                              .default = "c",
                              SIDweighted_answer = "d",
                              SIDraw = "d"))

  #filter to max year
  df_sid <- filter(df_sid, fiscal_year == max(fiscal_year))

  #store meta data
  metadata_sid <- list(caption = glue("Source: FY{max(df_sid$fiscal_year) %>% str_sub(-2)} SID Global Dataset"))

# LOAD UNAIDS -------------------------------------------------------------

  #store meta data
  metadata_unaids <- list(caption = glue("Source: {str_remove(source_note, '[:digit:]{4} ')} [released {str_extract(source_note, '[:digit:]{4}')}]"))
  
  #import
  df_unaids_tt <- pull_unaids(TRUE, "HIV Test & Treat")
  
  df_unaids_epi <- pull_unaids(FALSE, "epicontrol")
  
  df_epi_original <- pull_unaids(TRUE, "HIV Estimates")
  
# LOAD 10-10-10 ------------------------------------------------------------
  
  #store meta data
  metadata_pol_lab <- list(caption = "Source: HIV Policy Lab [2023-01-27]")

  #read in HIV Policy Lab data export
  df_tens <- googlesheets4::range_speedread(as_sheets_id(pol_lab_id), "Policy adoption data",
                                            skip = 6, col_types = "c") %>%
    janitor::clean_names()

