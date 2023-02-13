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

source(here("Scripts/91_setup.R"))


# GLOBAL VARIABLES --------------------------------------------------------

  # get_metadata()
  curr_pd <- "FY23Q1"
  
  # vct_cntry <- glamr::pepfar_country_list$country
  vct_cntry <- "Malawi"
  
# GENERATE REPORTS --------------------------------------------------------

  #clean names for filenaming (remove space and apostrophes)
  vct_cntry_clean <- gsub("( |\')", "", vct_cntry)
  
  #output files
  reports <- tibble(
    output_file = glue(here("markdown","{curr_pd}_{vct_cntry_clean}_cop-support-viz_oha-siei.html")),
    params = map(vct_cntry, ~list(cntry = ., agency = "PEPFAR"))
  )

    
  #create reports
  reports %>%
    pwalk(render,
          input = here("Scripts","country_reports.Rmd"))
