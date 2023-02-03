# PROJECT:  hardapoart
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  to develop a population pyramid viz
# REF ID:   aa8bd5b4 
# LICENSE:  MIT
# DATE:     2023-02-03
# UPDATED:  

# DEPENDENCIES ----------------------------------------------------------------
  library(tidyverse)
  library(extrafont)
  library(gagglr)
  library(glue)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)

# GLOBAL VARIABLES ------------------------------------------------------------
  
  ref_id <- "aa8bd5b4"
  
  get_metadata() #list of MSD metadata elements
  
  ou <- "South Sudan"
  agency <- "USAID"

# IMPORT ----------------------------------------------------------------------
  
  ou_path <- "OU_IM"
  
# FUNCTIONS -------------------------------------------------------------------
  
  pop_pyramid <- function(.path, .ou, ...){
    
    ou_df <- si_path() %>%
      return_latest(.path) %>%
      read_msd()
  
    df <- ou_df %>%
      resolve_knownissues() %>%
      filter(
        fiscal_year == metadata$curr_fy,
        operatingunit == .ou,
        indicator == "TX_CURR",
        standardizeddisaggregate == "Age/Sex/HIVStatus") %>%
      reshape_msd(direction = "quarters") %>%
      filter(period == metadata$curr_pd) %>%
      group_by(fiscal_year, period, funding_agency, indicator, sex, ageasentered) %>%
      summarise(across(results, sum, na.rm = TRUE)) %>%
      mutate(
        population = if_else(sex == "Male", results*(-1), results*1), 
        pop_label = abs(population))
    
    df %>%
      ggplot(aes(x = ageasentered, y = population, fill = sex)) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = c("Male" = genoa, 
                                   "Female" = moody_blue)) +
      scale_y_continuous(limits = c(-7000, 7000), 
                         breaks = seq(-7000, 7000, by = 1000),
                         # this is brittle
                         label = c("7k", "6k", "5k", "4k", "3k", "2k", "1k", 
                                   "0","1k", "2k","3k", "4k", "5k", "6k", "7k")) +
      labs(title = glue("Population Pyramid"),
           x = NULL, y = NULL, fill = NULL,
           subtitle = glue("{df$indicator[1]} | {metadata$curr_pd}"),
           caption = glue("Source: {metadata$curr_pd} MSD | Ref id: {ref_id} | US Agency for International Development")) +
      si_style_yline() +
      theme(
        panel.spacing = unit(.5, "line"),
        legend.position = "none",
        plot.title = element_markdown(),
        strip.text = element_markdown())
  }

# VIZ --------------------------------------------------------------------------
  
  pop_pyramid(ou_path, ou)
  
  si_save("Images/8_pop_pyramid.png")
  