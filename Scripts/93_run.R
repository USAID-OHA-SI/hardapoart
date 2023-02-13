# PROJECT:  hardapoart
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  iterate through country reports
# REF ID:   5bea2027 
# LICENSE:  MIT
# DATE:     2023-02-13
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(glue)
library(gophr)
library(here)
library(quarto)
library(rmarkdown)


# SETUP DATA --------------------------------------------------------------

# source(here("Scripts/91_setup.R"))
#run manually metadata causing issues with rmd - cannot change value of locked binding for 'metadata'

# GLOBAL VARIABLES --------------------------------------------------------

  # vct_cntry <- glamr::pepfar_country_list$country
  vct_cntry <- pepfar_country_list %>% 
    filter(operatingunit %in% c("Nigeria", 
                                "South Sudan",
                                "Tanzania",
                                "Mozambique",
                                "Ethiopia",
                                "South Africa",
                                "Western Hemisphere Region")) %>%
    pull(country)
  
  
# GENERATE REPORTS --------------------------------------------------------

  #clean names for filenaming (remove space and apostrophes)
  vct_cntry_clean <- gsub("( |\')", "", vct_cntry)
  
  #output files
  reports <- tibble(
    output_file = glue(here("markdown","{metadata_msd$curr_pd}_{vct_cntry_clean}_cop-support-viz_oha-siei.html")),
    params = map(vct_cntry, ~list(curr_pd = metadata_msd$curr_pd, cntry = ., agency = "PEPFAR"))
  )

    
  #create reports
  reports %>%
    pwalk(render,
          input = here("Scripts","country_report.Rmd"))
