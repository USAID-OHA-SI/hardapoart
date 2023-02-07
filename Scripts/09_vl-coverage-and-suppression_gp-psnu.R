##  PROJECT: <Project covered by this repository>
##  AUTHOR:  Baboyma Kagniniwa | USAID
##  PURPOSE: HIV Prevalence by geographies and gender
##  REF. ID: 29675452
##  LICENCE: MIT
##  DATE:    2023-02-12
##  UPDATE:  2023-02-12

# LIBRARIES ----
  
  library(tidyverse)
  library(glamr)
  library(gophr)
  library(grabr)
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
  ref_id <- "29675452"
  
  authors <- c("A.Chatfez",
               "B.Kagniniwa", 
               "J.Hoehner",
               "K.Srikanth", 
               "N.Petrovic",
               "T.Essam")
  
  # SI Backstop Coverage
  
  cntry <- "Nigeria"
  
  agency <- "USAID"
  
  ## Dirs 
  
  dir_mer <- si_path(type = "path_msd")
  dir_out <- "./Dataout"
  dir_imagess <- "./Images"
  dir_graphics <- "./Graphics"
  
  ## Files
  
  file_psnu <- dir_mer %>% 
    return_latest("PSNU_IM")
  
  ## Info
  
  src_msd <- source_info(file_psnu)
  
  curr_fy <- source_info(file_psnu, return = "fiscal_year")
  curr_qtr <- source_info(file_psnu, return = "quarter")
  curr_pd <- source_info(file_psnu, return = "period")
  
  ## Tech Areas / Disaggs
  
  inds_vl <- c("TX_CURR", "TX_PVLS")
  disaggs_vl <- c("Age/Sex/HIVStatus", "Age/Sex/Indication/HIVStatus")
  
# FUNCTIONS ----

# DATA IMPORT ----
  
  # PEPFAR Program Data
  
  df_psnu <- file_psnu %>% read_msd()
  
# MUNGING ----
  
  ## TX Indicators
  
  df_tx <- df_psnu %>% 
    filter(
      fiscal_year %in% c(curr_fy, curr_fy - 1),
      funding_agency != "Dedup", 
      indicator %in% inds_vl,
      standardizeddisaggregate %in% disaggs_vl
    ) %>% 
    mutate(indicator = case_when(
        indicator == "TX_PVLS" & numeratordenom == "D" ~ paste0(indicator, "_D"),
        TRUE ~ indicator)
    ) 
  
  df_tx <- df_tx %>%
    select(-cumulative, -targets) %>% 
    group_by(fiscal_year, funding_agency, operatingunit, indicator, snu1) %>%
    #summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>%
    #summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") %>%
    summarise(qtr1 = sum(qtr1, na.rm = TRUE), 
              qtr2 = sum(qtr2, na.rm = TRUE),
              qtr3 = sum(qtr3, na.rm = TRUE),
              qtr4 = sum(qtr4, na.rm = TRUE),
              .groups = "drop") 
  
  df_tx <- df_tx %>% reshape_msd()
  
  
  ## VL Calculation
  
  df_vl <- df_tx %>% 
    select(-period_type) %>% 
    pivot_wider(names_from = indicator, values_from = value) %>% 
    rename_with(str_to_lower) %>% 
    group_by(funding_agency, operatingunit, snu1) %>% 
    mutate(
      vlc = tx_pvls_d / dplyr::lag(tx_curr, 2, order_by = period),
      vls = tx_pvls / tx_pvls_d
    ) %>% 
    ungroup()
  
  ## Limits history to last 5
  
  hist_pds <- df_vl %>% 
    distinct(period) %>% 
    arrange(desc(period)) %>% 
    pull() %>% 
    magrittr::extract(1:5) %>% 
    sort()
  
  df_vl <- df_vl %>% filter(period %in% hist_pds)
  
    
# VIZ ----

  df_vl %>% 
    filter(funding_agency == agency, 
           operatingunit == cntry) %>% 
    ggplot(aes(x = period, group = 1)) +
    geom_line(aes(y = vlc), color = burnt_sienna, linewidth = 1) +
    geom_point(aes(y = vlc), fill = burnt_sienna, color = grey10k, shape = 21, size = 4) +
    geom_text(aes(y = vlc, label = percent(vlc, 1)), vjust = -1.5, color = burnt_sienna) +
    geom_line(aes(y = vls), color = genoa, linewidth = 1) +
    geom_point(aes(y = vls), fill = genoa, color = grey10k, shape = 21, size = 4) +
    geom_text(aes(y = vls, label = percent(vls, 1)), vjust = 1.8, color = genoa) +
    scale_y_continuous(labels = percent) +
    labs(x = "", y = "",
         title = glue::glue("{toupper(cntry)} - VIRAL LOAD TRENDS"),
         subtitle = glue::glue("<span style='color:{burnt_sienna}'>Coverage</span> & <span style='color:{genoa}'>Supression</span>"),
         caption = glue::glue("Source: {src_msd} - Created by OHA/SIEI | Ref. ID #{ref_id}")) +
    facet_wrap(~snu1) +
    si_style_nolines() +
    theme(plot.title = element_markdown(),
          plot.subtitle = element_markdown(),
          axis.text.y = element_blank(),
          strip.text = element_text(size = 12, face = "bold"))
  
  df_vl %>% 
    filter(funding_agency == agency,
           str_detect(operatingunit, "Region$", negate = T)) %>% 
    distinct(operatingunit) %>% 
    pull() %>% 
    last() %>% 
    walk(function(.ou) {
      viz <- df_vl %>% 
        filter(funding_agency == agency,
               operatingunit == .ou) %>% 
        ggplot(aes(x = period, group = 1)) +
        geom_line(aes(y = vlc), color = burnt_sienna, linewidth = 1) +
        geom_point(aes(y = vlc), fill = burnt_sienna, color = grey10k, shape = 21, size = 4) +
        geom_text(aes(y = vlc, label = percent(vlc, 1)), vjust = 2, color = burnt_sienna) +
        geom_line(aes(y = vls), color = genoa, linewidth = 1) +
        geom_point(aes(y = vls), fill = genoa, color = grey10k, shape = 21, size = 4) +
        geom_text(aes(y = vls, label = percent(vls, 1)), vjust = -2, color = genoa) +
        scale_y_continuous(labels = percent) +
        labs(x = "", y = "",
             title = glue::glue("{toupper(.ou)} - VIRAL LOAD TRENDS"),
             subtitle = glue::glue("<span style='color:{burnt_sienna}'>Coverage</span> & <span style='color:{genoa}'>Supression</span>"),
             caption = glue::glue("Source: {src_msd} - Created by OHA/SIEI | Ref. ID #{ref_id}")) +
        facet_wrap(~snu1) +
        si_style_nolines() +
        theme(plot.title = element_markdown(),
              plot.subtitle = element_markdown(),
              axis.text.y = element_blank(),
              strip.text = element_text(size = 12, face = "bold"))
      
      print(viz)
      
      si_save(plot = viz,
              filename = glue::glue("./Graphics/{curr_pd} - {toupper(.ou)} VLCS Trends by SNU1"))
    })
  
# EXPORT ----
  