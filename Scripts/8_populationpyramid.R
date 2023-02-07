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
  
  # testing with small and large OU portfolios
  # ou <- "Zambia"
  ou <- "South Sudan"

# IMPORT ----------------------------------------------------------------------
  
  nat_subnat_path <- "NAT_SUBNAT"

# MUNGE ------------------------------------------------------------------------
  
  df <- si_path() %>%
    return_latest(nat_subnat_path) %>%
    read_msd()
  
  df_filt <- df %>%
    filter(
      fiscal_year == metadata$curr_fy,
      operatingunit == ou,
      indicator == "PLHIV") %>%
    select(fiscal_year, operatingunit, indicator, sex, ageasentered, targets) %>%
    group_by(fiscal_year, operatingunit, indicator, sex, ageasentered) %>%
    summarise(across(targets, \(x) sum(x, na.rm = TRUE))) %>%
    mutate(
      population = if_else(sex == "Male", targets*(-1), targets*1))
  
  unknown <- df_filt  %>%
    filter(is.na(sex) & is.na(ageasentered)) %>%
    mutate(n_unknown = comma(targets))
  
  df_viz <- df_filt %>%
    drop_na(sex, ageasentered)
  
# VIZ --------------------------------------------------------------------------
  
  df_viz %>%
    ggplot(aes(x = ageasentered, y = population, fill = sex)) +
    geom_col() +
    coord_flip() +
    scale_fill_manual(values = c("Male" = genoa, 
                                 "Female" = moody_blue)) +
    scale_y_continuous(
                       # would be great to have it 
                       # dynamically choose a scale 
                       # based on the length of "value" since this can vary by OU 
                       limits = c(min(df_viz$population), max(df_viz$population)),
                       labels = function(x) {glue("{comma(abs(x))}")}, 
                       # would like it to be able to use this ideally but
                       # can't figure out how to use this and abs together
                       # label_number(scale_cut = cut_short_scale())
                      ) +
    labs(title = glue("Population Pyramid"),
         x = NULL, y = NULL, fill = NULL,
         subtitle = glue("{df_viz$indicator[1]} | {metadata$curr_fy_lab}"),
         caption = 
           glue("Note: There are {unknown$n_unknown[1]} PLHIV with unreported age and sex data.
                  Source: {metadata$curr_pd} MSD | Ref id: {ref_id} | US Agency for International Development")) +
    si_style_yline() +
    theme(
      panel.spacing = unit(.5, "line"),
      legend.position = "none",
      plot.title = element_markdown(),
      strip.text = element_markdown())
  
  si_save("Images/8_pop_pyramid.png")
  