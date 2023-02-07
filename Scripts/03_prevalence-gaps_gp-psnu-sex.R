##  PROJECT: <Project covered by this repository>
##  AUTHOR:  Baboyma Kagniniwa | USAID
##  PURPOSE: HIV Prevalence by geographies and gender
##  REF. ID: 8fb89847
##  LICENCE: MIT
##  DATE:    2023-02-02
##  UPDATE:  2023-02-02

# LIBRARIES ----
  
  library(tidyverse)
  library(glamr)
  library(gophr)
  library(grabr)
  library(mindthegap)
  library(glitr)
  library(extrafont)
  library(scales)
  library(tidytext)
  library(ggtext)
  library(patchwork)
  
# NOTES ----

# DISCLAIMERS ----

# GLOBAL PARAMS ----
  
  ## Script Reference
  ref_id <- "8fb89847"
  
  authors <- c("A.Chatfez",
               "B.Kagniniwa", 
               "J.Hoehner",
               "K.Srikanth", 
               "N.Petrovic",
               "T.Essam")
  
  # SI Backstop Coverage
  cntry = "Nigeria"
  
  ## Dirs 
  
  dir_mer <- si_path(type = "path_msd")
  dir_out <- "./Dataout"
  dir_imagess <- "./Images"
  dir_graphics <- "./Graphics"
  
  ## Files
  
  file_subnat <- dir_mer %>% 
    return_latest("NAT_SUBNAT")
  
  file_psnu <- dir_mer %>% 
    return_latest("PSNU_IM")
  
  ## Info
  
  src_msd <- source_info(file_psnu)
  
  curr_fy <- source_info(file_psnu, return = "fiscal_year")
  curr_qtr <- source_info(file_psnu, return = "quarter")
  curr_pd <- source_info(file_psnu, return = "period")
  
  ## Tech Areas / Disaggs
  
  inds_pops <- c("POP_EST", "PLHIV")
  disaggs_pops <- c("Age/Sex", "Age/Sex/HIVStatus")
  
# FUNCTIONS ----

# DATA IMPORT ----
  
  # UNAIDS Country Stats
  
  # df_prev <- pull_unaids(data_type = "HIV Estimates", pepfar_only = TRUE)
  #   
  # df_prev %>% glimpse()
  # df_prev %>% distinct(year) %>% prinf()
  # df_prev %>% distinct(indicator)
  
  # PEPFAR Program Data
  
  df_subnat <- file_subnat %>% read_msd()
  
  df_subnat %>% glimpse()
  df_subnat %>% distinct(fiscal_year)
  df_subnat %>% distinct(indicator)
  
  df_subnat %>% 
    filter(indicator %in% c("PLHIV", "POP_EST")) %>% 
    count(fiscal_year, indicator, standardizeddisaggregate, wt = targets) %>% 
    filter(n != 0) %>% 
    pivot_wider(names_from = fiscal_year, values_from = n) %>% 
    prinf()
  
# MUNGING ----
  
  df_subnat %>% 
    filter(indicator %in% inds) %>% 
    count(fiscal_year, indicator, standardizeddisaggregate, wt = targets) %>% 
    filter(n != 0) %>% 
    pivot_wider(names_from = fiscal_year, values_from = n) %>% 
    prinf()
  
  ## Age/Sex Summaries
  
  df_pops <- df_subnat %>% 
    filter(fiscal_year == curr_fy,
           indicator %in% inds_pops,
           standardizeddisaggregate %in% disaggs_pops) 
  
  df_pops <- df_pops %>% 
    group_by(operatingunit, country, snu1uid, snu1,
             psnuuid, psnu, indicator, ageasentered, sex) %>% 
    summarise(value = sum(targets, na.rm = T), .groups = "drop")
  
  ## Add OU/Country Summary
  
  df_pops <- df_pops %>% 
    group_by(operatingunit, country, indicator, ageasentered, sex) %>% 
    summarise(value = sum(value, na.rm = T), .groups = "drop") %>% 
    mutate(psnu = "COUNTRY") %>% 
    bind_rows(df_pops, .)
  
  df_pops <- df_pops %>% 
    filter(psnu != "COUNTRY") %>% 
    group_by(operatingunit, indicator, ageasentered, sex) %>% 
    summarise(value = sum(value, na.rm = T), .groups = "drop") %>% 
    mutate(psnu = "OU") %>% 
    bind_rows(df_pops, .)
  
  # PSNU Summaries 
  df_pops_psnu <- df_pops %>% 
    group_by(operatingunit, country, 
             snu1uid, snu1, psnuuid, psnu, indicator) %>% 
    summarise(value = sum(value, na.rm = T), .groups = "drop")
  
  # Sex Summaries 
  df_pops_sex <- df_pops %>% 
    group_by(operatingunit, country,
             snu1uid, snu1, psnuuid, psnu, indicator, sex) %>% 
    summarise(value = sum(value, na.rm = T), .groups = "drop") 
  
  ## Calculate HIV Prevalence by sex
  
  df_prev_sex <- df_pops_sex %>% 
    clean_psnu() %>% 
    group_by(operatingunit, country, snu1uid, snu1, psnuuid, psnu, sex) %>% 
    reframe(prevalence = value[indicator == "PLHIV"] / 
              value[indicator == "POP_EST"]) %>% 
    ungroup() 
  
  df_prev_psnu <- df_pops_sex %>% 
    clean_psnu() %>% 
    group_by(operatingunit, country, snu1uid, snu1, psnuuid, psnu) %>% 
    reframe(psnu_prev = sum(value[indicator == "PLHIV"], na.rm = T) / 
              sum(value[indicator == "POP_EST"], na.rm = T)) %>% 
    ungroup() 
  
  df_prev_sex <- df_prev_sex %>% 
    left_join(df_prev_psnu,
              by = c("operatingunit", "country", 
                     "snu1uid", "snu1", "psnuuid", "psnu"))
  
  ## Apply OHA Style - Gender Colors
  
  df_prev_sex_gap <- df_prev_sex %>% 
    select(-psnu_prev) %>% 
    mutate(sex = tolower(sex)) %>% 
    pivot_wider(names_from = sex,
                values_from = prevalence) %>% 
    mutate(color_gap = grey30k)
  
  df_prev_sex <- df_prev_sex %>% 
    left_join(df_prev_sex_gap,
              by = c("operatingunit", "country", 
                     "snu1uid", "snu1", "psnuuid", "psnu")) %>% 
    mutate(
      color_sex = case_when(
        sex == "Female" ~ moody_blue,
        sex == "Male" ~ genoa,
        TRUE ~ grey30k
      ),
      psnu_label = case_when(
        psnu %in% c("COUNTRY", "OU") ~ paste0("<span style='color:", usaid_black, "'><strong>", psnu, "</strong></span>"),
        TRUE ~ psnu
      ),
    ) %>% 
    group_by(operatingunit) %>% 
    mutate(
      threshold = case_when(
        psnu_prev < prevalence[psnu == "OU"] ~ .3,
        TRUE ~ 1
      )
    )
    
    
# VIZ ----

  # HIV Prevalence by Gender ----
  
  df_prev_sex %>% 
    filter(country == cntry) %>% 
    ggplot(data = ., 
           aes(x = reorder(psnu, female), 
               y = prevalence,
               fill = color_sex)) +
    geom_hline(yintercept = seq(from = 0, 
                                to =  df_prev_sex %>% 
                                  filter(country == cntry) %>% 
                                  pull(prevalence) %>%
                                  max() %>%
                                  round(2), 
                                by = 0.01),
               size = .8, linetype = "dashed", color = grey20k) +
    geom_vline(xintercept = "COUNTRY",
               size = .8, linetype = "dashed", color = usaid_darkgrey) +
    geom_segment(aes(xend = reorder(psnu, female),
                     y = female, 
                     yend = male,
                     color = color_gap),
                 linewidth = 2) +
    geom_point(shape = 21, size = 5, color = grey10k) +
    scale_fill_identity() +
    scale_color_identity() +
    scale_y_continuous(labels = percent, position = "right") +
    coord_flip() +
    labs(x = "", y = "", 
         title = glue::glue("{toupper(cntry)} - {curr_fy} HIV PREVALANCE"),
         subtitle = glue::glue("Prevalence Gap between <span style='color:{moody_blue}'>Female</span> & <span style='color:{genoa}'>Male</span> by PSNU"),
         caption = glue::glue("Source: {src_msd} - Created by OHA/SIEI | Ref. ID #{ref_id}")) +
    si_style_nolines() +
    theme(plot.subtitle = element_markdown(),
          axis.text.y = element_markdown())
  
  
  df_prev_sex %>% 
    filter(str_detect(operatingunit, "Region$", negate = TRUE),
           psnu != "COUNTRY") %>% 
    distinct(operatingunit) %>% 
    pull() %>% 
    first() %>% 
    walk(function(.ou) {
      print(.ou)
      
      # Guides
      gap_max <- df_prev_sex %>% 
        filter(operatingunit == .ou,
               psnu != "COUNTRY") %>% 
        pull(prevalence) %>%
        max() %>%
        round(2)
      
      gap_step <- .01
      
      if (gap_max > .01) gap_step <- .05
        
      # Viz
      viz <- df_prev_sex %>% 
        filter(operatingunit == .ou) %>% 
        ggplot(data = ., 
               aes(x = reorder(psnu, female), 
                   y = prevalence,
                   fill = color_sex)) +
        geom_hline(yintercept = seq(from = 0, 
                                    to = gap_max, 
                                    by = gap_step),
                   size = .8, linetype = "dashed", color = grey20k) +
        geom_vline(xintercept = "OU",
                   size = .8, linetype = "dashed", color = usaid_darkgrey) +
        geom_segment(aes(xend = reorder(psnu, female),
                         y = female, 
                         yend = male,
                         color = color_gap),
                     linewidth = 2) +
        geom_point(shape = 21, size = 5, color = grey10k) +
        scale_fill_identity() +
        scale_color_identity() +
        scale_y_continuous(labels = percent, position = "right") +
        coord_flip() +
        labs(x = "", y = "", 
             title = glue::glue("{toupper(.ou)} - {curr_fy} HIV PREVALANCE"),
             subtitle = glue::glue("Prevalence Gap between <span style='color:{moody_blue}'>Female</span> & <span style='color:{genoa}'>Male</span> by PSNU"),
             caption = glue::glue("Source: {src_msd} - Created by OHA/SIEI | Ref. ID #{ref_id}")) +
        si_style_nolines() +
        theme(plot.subtitle = element_markdown(),
              axis.text.y = element_markdown())
      
      print(viz)
      
      si_save(plot = viz,
              filename = glue::glue("{curr_pd} - {toupper(.ou)} HIV Prevalence.png"))
    })
  
  
  
# EXPORT ----
  