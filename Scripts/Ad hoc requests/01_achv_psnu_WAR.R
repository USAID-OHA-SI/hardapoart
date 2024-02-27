# PROJECT:  hardapoart
# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  iterate through achv visuals for WAR request
# REF ID:   cb320288 
# LICENSE:  MIT
# DATE:     2024-02-27
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(glue)
library(gophr)
library(here)
library(quarto)
library(rmarkdown)

ref_id <- "cb320288"

msd_path <- si_path() %>%
  return_latest("MER_Structured_Datasets_PSNU_IM_FY21-24_20231215_v2_1_West Africa Region")

#store meta data
get_metadata(msd_path)
metadata_msd <- metadata
rm(metadata)

#import
df_msd <- si_path() %>%
  return_latest("MER_Structured_Datasets_PSNU_IM_FY21-24_20231215_v2_1_West Africa Region") %>%
  read_psd()

#add _D to denom variables
df_msd <- clean_indicator(df_msd)

#MSD filter table
df_msd_ind <-
  tibble::tribble(
    ~indicator,        ~standardizeddisaggregate,
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
    "TB_PREV_D",              "Total Denominator",
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



# SETUP DATA --------------------------------------------------------------

#need to store a placeholder for metadata, to avoid conflict with rmarkdown metadata
metadata <- ""

#load all data/metadata info
source(here("Scripts/01_achievement_indicator-psnu.R"))


#remove placeholder
rm(metadata)


export_achv_img <- function(cntry, agency, folder_out = "Images"){
  
  fldr <- file.path(folder_out, cntry)
  
  if(fs::dir_exists(fldr))
    fs::dir_delete(fldr)
  
  fs::dir_create(fldr)
  
  #10s_barriers
  df_msd %>%
    prep_achv_psnu(cntry, agency) %>%
    viz_achv_psnu()
  
  
  si_save(file.path(fldr, glue("{length(list.files(fldr, 'png')) + 1}_{cntry}_achv_FY23Q4")), device = "png")
  
}

# GLOBAL VARIABLES --------------------------------------------------------

#get around metadata lcoking error
metadata_msd$curr_pd <- "FY23Q4"
metadata_msd$curr_qtr <- 4
metadata_msd$curr_fy <- 2023
metadata_msd$source <- "FY23Q4c MSD"

vct_cntry <- glamr::pepfar_country_list %>% 
  filter(operatingunit == "West Africa Region") %>% 
  pull(country)

#test one
df_msd %>%
  prep_achv_psnu(vct_cntry[2], "PEPFAR") %>%
  viz_achv_psnu()

# GENERATE IMAGES --------------------------------------------------------

#export folder of images
walk(vct_cntry, 
     ~export_achv_img(.x, "PEPFAR"))
