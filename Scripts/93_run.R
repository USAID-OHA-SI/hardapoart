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


source("Scripts/98_export_imgs.R")

# SETUP DATA --------------------------------------------------------------

# source(here("Scripts/91_setup.R"))
#run manually metadata causing issues with rmd - cannot change value of locked binding for 'metadata'

# GLOBAL VARIABLES --------------------------------------------------------

  vct_cntry <- glamr::pepfar_country_list$country

  
# GENERATE REPORTS --------------------------------------------------------

  #clean names for filenaming (remove space and apostrophes)
  vct_cntry_clean <- gsub("( |\')", "", vct_cntry)
  
  #output files
  reports <- tibble(
    output_file = glue(here("markdown","{metadata_msd$curr_pd}_{vct_cntry_clean}_cop-support-viz_oha-siei.html")),
    params = map(vct_cntry, ~list(curr_pd = metadata_msd$curr_pd, cntry = ., agency = "PEPFAR"))
  )
  
  # reports <- tibble(
  #   output_file = glue("{metadata$curr_pd}_{vct_cntry}_cop-support-viz_oha-siei.pptx"),
  #   params = map(vct_cntry, ~list(curr_pd = metadata$curr_pd, cntry = ., agency = "PEPFAR"))
  # )
  

    
  #create reports
  reports %>%
    pwalk(render,
          input = here("Scripts","country_report.Rmd"))

  # reports %>%
  #   filter(str_detect(output_file, "Nigeria")) %>% 
  #   pwalk(function(output_file, params) {
  #     quarto_render(
  #       #input = here("Scripts", "country_report.qmd"),
  #       input = here("hardapoart.qmd"),
  #       output_file = output_file,
  #       execute_params = params
  #     )
  #   })
  
  
  vct_cntry
  
  export_imgs("Malawi", "PEPFAR")
