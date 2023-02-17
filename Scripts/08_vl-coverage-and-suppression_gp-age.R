##  PROJECT: <Project covered by this repository>
##  AUTHOR:  Baboyma Kagniniwa | USAID
##  PURPOSE: HIV Prevalence by geographies and gender
##  REF. ID: 29675452
##  LICENCE: MIT
##  DATE:    2023-02-12
##  UPDATE:  2023-02-13

# LIBRARIES ----
  
  # library(tidyverse)
  # library(glamr)
  # library(gophr)
  # library(grabr)
  # library(glitr)
  # library(extrafont)
  # library(scales)
  # library(tidytext)
  # library(ggtext)
  # library(patchwork)
  
# NOTES ----

# DISCLAIMERS ----

# GLOBAL PARAMS ----
  
  ## Script Reference
  # ref_id <- "29675452"
  
# FUNCTIONS ----

  #' @title Prep TX VL Datasets
  #' 
  prep_viral_load <- function(df, cntry, agency, 
                               pd_hist = 5) {
    
    #clean exit if no data
    if(cntry %ni% unique(df$country) | agency %ni% unique(df$funding_agency))
      return(NULL)
    
    # Filter
    df_tx <- df %>% 
      filter(
        funding_agency == agency, 
        country == cntry,
        indicator %in% c("TX_CURR", "TX_PVLS", "TX_PVLS_D"),
        standardizeddisaggregate %in% c("Age/Sex/HIVStatus", 
                                        "Age/Sex/Indication/HIVStatus")
      ) 
    
    if(nrow(df_tx) == 0)
      return(NULL)
             
    # Summarise results by age - bands
    df_tx <- df_tx %>%
      select(-cumulative, -targets) %>% 
      filter(ageasentered != "Unknown Age") %>% 
      mutate(age = case_when(
        trendscoarse == "<15" ~ trendscoarse,
        ageasentered == "15-19" ~ ageasentered,
        ageasentered %in% "20-24" ~ ageasentered,
        TRUE ~ "25+"
      )) %>% 
      group_by(fiscal_year, funding_agency, operatingunit, country, indicator, age) %>%
      #summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>%
      #summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") %>%
      summarise(qtr1 = sum(qtr1, na.rm = TRUE), 
                qtr2 = sum(qtr2, na.rm = TRUE),
                qtr3 = sum(qtr3, na.rm = TRUE),
                qtr4 = sum(qtr4, na.rm = TRUE),
                .groups = "drop") 
    
    # Reshape long and calculate VLC/S
    df_vl <- df_tx %>% 
      reshape_msd() %>% 
      select(-period_type) %>% 
      pivot_wider(names_from = indicator, values_from = value) %>% 
      rename_with(str_to_lower) %>% 
      group_by(funding_agency, operatingunit, country, age) %>% 
      mutate(
        vlc = tx_pvls_d / dplyr::lag(tx_curr, 2, order_by = period),
        vls = tx_pvls / tx_pvls_d
      ) %>% 
      ungroup()
    
    ## Limits history to last 5 quaters
    
    hist_pds <- df_vl %>% 
      distinct(period) %>% 
      arrange(desc(period)) %>% 
      pull() 
    
    # reset pd hisory to 4 for anything outside of 2:8
    if (pd_hist <= 1 | pd_hist > length(hist_pds)) {
      usethis::ui_warn(glue::glue("History length ({pd_hist}) is behind 1 and {length(hist_pds)}. Value was reset to 4."))
      pd_hist <- 4
    }
    
    hist_pds <- hist_pds %>% 
      magrittr::extract(1:pd_hist) %>% 
      sort()
    
    df_vl %>% filter(period %in% hist_pds)
  }
  
  #' @title Viz TX VL
  #' 
  viz_viral_load <- function(df, save = F) {
    
    if(is.null(df) || nrow(df) == 0)
      return(print(paste("No data available.")))
    
    ref_id <- "29675452"
    vrsn <- 1 
    
    viz <- df %>% 
      ggplot(aes(x = period, group = 1)) +
      geom_line(aes(y = vlc), color = burnt_sienna, linewidth = 1, na.rm = TRUE) +
      geom_point(aes(y = vlc), fill = burnt_sienna, color = grey10k, shape = 21, size = 4, na.rm = TRUE) +
      geom_text_repel(aes(y = vlc, label = percent(vlc, 1)), 
                      color = burnt_sienna, max.overlaps = 50, force = 10,
                      family = "Source Sans Pro", na.rm = TRUE) +
      geom_line(aes(y = vls), color = genoa, linewidth = 1, na.rm = TRUE) +
      geom_point(aes(y = vls), fill = genoa, color = grey10k, shape = 21, size = 4, na.rm = TRUE) +
      geom_text_repel(aes(y = vls, label = percent(vls, 1)), 
                      color = genoa, max.overlaps = 50, force = 10,
                      family = "Source Sans Pro", na.rm = TRUE) +
      scale_y_continuous(labels = percent) +
      labs(x = "", y = "",
           title = glue::glue("{toupper(unique(df$funding_agency))}/{toupper(unique(df$country))} - VIRAL LOAD TRENDS"),
           subtitle = glue::glue("<span style='color:{burnt_sienna}'>Coverage</span> & <span style='color:{genoa}'>Suppression</span>"),
           caption = glue::glue("{metadata_msd$caption} | USAID/OHA/SIEI |  Ref id: {ref_id} v{vrsn}")) +
      coord_cartesian(clip = "off") +
      facet_wrap(~age) +
      si_style_nolines() +
      theme(plot.title = element_markdown(),
            plot.subtitle = element_markdown(),
            axis.text.y = element_blank(),
            strip.text = element_text(size = 12, face = "bold"),
            strip.clip = "off",
            strip.placement = "outside",
            panel.spacing = unit(0, "lines"))
    
    print(viz)
    
    if (save) {
      glitr::si_save(
        plot = viz,
        filename = glue::glue("./Graphics/{max(df$period)} - {toupper(unique(df$country)} VLCS Trends by Age Group.png"))
    }
    
  }
  
# DATA IMPORT ----
  
  # PEPFAR Program Data
    
# # VIZ ----

  # Agency
  # pepfar_country_list %>% 
  #   pull(country) %>% 
  #   #first() %>% 
  #   nth(26) %>% 
  #   #nth(28) %>% 
  #   #nth(46) %>% 
  #   prep_viral_load(df = df_msd,
  #                   agency = "USAID",
  #                   cntry = .,
  #                   pd_hist = 5) %>%
  #     viz_viral_load()
  # 
  # # All "PEPFAR"
  # 
  # prep_viral_load(df = df_msd,
  #                 agency = "PEPFAR",
  #                 cntry = "Nigeria",
  #                 pd_hist = 5) %>%
  #   viz_viral_load()
# 
#   
# # EXPORT ----
#   
