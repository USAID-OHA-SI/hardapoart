# PROJECT:  hardapoart
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  gaps identified by SID
# REF ID:   79fe7ff5 
# LICENSE:  MIT
# DATE:     2023-02-02
# UPDATED: 
# NOTE:     based on catch-22/0220708_SID-scores-across-sutained-impact-cntries.R

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
  
  ref_id <- "79fe7ff5" #id for adorning to plots, making it easier to find on GH
  
  #SID_Global_Dataset Final 2.0.xlsx
  gs_id <- as_sheets_id("1nn4c9NBsYchD6xUjimbWBB-4tHvrc4-AnWntGOk0XLc")
  #use for QC - https://tableau.usaid.gov/views/GlobalAnalysis/ElementBoxPlot/ae453af9-0cc3-4a46-9e78-e4fa706e48d1/6065460b-c329-480e-9cc6-ba4e75b9566b?:display_count=n&:showVizHome=n&:origin=viz_share_link

  cntry <- "Malawi"

# IMPORT ------------------------------------------------------------------
  
  #see Documents/Data_Sources.md for info
  df_sid <- range_speedread(gs_id,
                            col_types = c(
                              .default = "c",
                              SIDweighted_answer = "d",
                              SIDraw = "d")
                            )
  
# MUNGE -------------------------------------------------------------------

  #limit dataset
  df_sid <- df_sid %>% 
    select(fiscal_year,  
           countryname = operatingunit,
           uuid = UUID,
           sid_area = SIDQuestion,
           sid_short_q = SIDshortquestion,
           sid_score_raw = SIDraw,
           sid_score_weighted = SIDweighted_answer) %>% 
    filter(fiscal_year == max(fiscal_year),
           !is.na(sid_score_raw))

  #clean sid area
  df_sid <- df_sid %>% 
    mutate(sid_area = str_remove_all(sid_area, "^[:digit:]{1,2}\\. |:$") %>% str_trim)
  
  #average score for each area by country
  df_sid_weighted <- df_sid %>% 
    group_by(fiscal_year, countryname, sid_area) %>% 
    summarise(avg_sid_score_weighted = sum(sid_score_weighted, na.rm = TRUE),
              .groups = "drop")
  
  
  #create point for country, PEPFAR avg, and bounds for plotting
  df_viz <- df_sid_weighted %>% 
    mutate(val_cntry = case_when(countryname == cntry ~ avg_sid_score_weighted)) %>% 
    group_by(sid_area) %>% 
    mutate(lower = min(avg_sid_score_weighted),
           upper = max(avg_sid_score_weighted),
           avg = mean(avg_sid_score_weighted),
           lab = case_when(countryname == cntry ~ avg_sid_score_weighted)) %>% 
    ungroup() 
  
  #color and group areas
  df_viz <- df_viz %>% 
    group_by(sid_area) %>% 
    mutate(fill_color = case_when(countryname == cntry & avg_sid_score_weighted > avg ~ genoa,
                                  countryname == cntry ~ moody_blue,
                                  TRUE ~ trolley_grey_light),
           font_color = ifelse(lab == max(lab, na.rm = TRUE), "white", matterhorn),
           fct_grp = ifelse(val_cntry > avg, "Above PEPFAR Average", "Below PEPFAR Average")) %>%
    fill(fct_grp, .direction = "downup") %>% 
    ungroup() 
  
  df_viz <- df_viz %>% 
    mutate(distance = val_cntry - avg)

# VIZ ---------------------------------------------------------------------

  df_viz %>% 
    ggplot(aes(avg_sid_score_weighted, fct_reorder(sid_area, val_cntry, sum, .na_rm = TRUE))) +
    # ggplot(aes(avg_sid_score_weighted, fct_reorder(sid_area, avg_sid_score_weighted, mean, na.rm = TRUE))) +
    geom_linerange(aes(xmin = lower, xmax = upper), color = grey10k) +
    geom_point(color = "white", alpha = .2, na.rm = TRUE) +
    geom_point(alpha = .2, na.rm = TRUE) +
    geom_errorbar(aes(xmin = avg, xmax = avg), color = matterhorn) +
    geom_point(aes(x = val_cntry, fill = fill_color), color = matterhorn, shape = 21, size = 6, na.rm = TRUE) +
    geom_text(aes(x = val_cntry, label = number(lab, .1), color = font_color),
              na.rm = TRUE,
              family = "Source Sans Pro", size = 8/.pt) +
    # facet_grid(fct_grp~., scales = "free_y", space = "free") +
    expand_limits(x = c(0, 10)) +
    scale_x_continuous(expand = c(.005, .005)) +
    coord_cartesian(clip = "off") +
    scale_fill_identity() +
    scale_color_identity() +
    labs(x = NULL, y = NULL,
         subtitle = glue("{unique(df_viz$fiscal_year)} SID Average Scores | large points represent {cntry}'s score compared with other PEPFAR countries (smaller points) and PEPFAR average (line)"),
         caption = glue("Source: SID Global Dataset (2.0) | USAID | Ref id: {ref_id}")) +
    si_style_xgrid(facet_space = .5) +
    theme(axis.text.x = element_blank())
  
  si_save("Images/11_sid_comp.png")
  