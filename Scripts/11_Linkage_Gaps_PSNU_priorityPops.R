# PROJECT:  hardapoart
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  to visualize linkage across psnus, gaps for priority pops
# REF ID:   f6f26589 
# LICENSE:  MIT
# DATE:     2023-02-07
# UPDATED: 

# DEPENDENCIES ----------------------------------------------------------------
  
  library(gagglr)
  library(tidyverse)
  library(scales)
  library(sf)
  library(glue)
  library(extrafont)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(selfdestructin5)
  library(gt)
  library(cascade) # Use dev version
  library(ggpattern)
  library(rcartocolor)

# GLOBAL VARIABLES ------------------------------------------------------------
  
  ref_id <- "f6f26589"
  path <- "Genie_PSNU_IM_Democratic_Republic_of_the_Congo_Frozen"

# IMPORT ----------------------------------------------------------------------
  
  df <- si_path() %>%
    return_latest(path) %>%
    read_psd()
  
  load_secrets()
  get_metadata()

# MUNGE -----------------------------------------------------------------------
  
  df_msd <- df  %>% 
   # filter(operatingunit == ou) %>%
    clean_agency() %>%
    resolve_knownissues()
  
  # Linkage Trends
  # National roll-up:
  
  df_hts_nat_base <- df_msd %>% 
    filter(indicator %in% c("HTS_TST_POS", "TX_NEW", "HTS_TST"), 
           standardizeddisaggregate == "Total Numerator",
           fiscal_year == metadata$curr_fy,
           funding_agency == "USAID") %>% 
    group_by(indicator, fiscal_year) %>% 
    summarise(across(targets:qtr4, \(x) sum(x, na.rm = T)), 
              .groups = "drop") %>% 
    reshape_msd(direction ="semi-wide") %>% 
    group_by(indicator) %>% 
    fill(targets, .direction = "down") %>% 
    filter(nchar(period) != 4, 
           period == metadata$curr_pd) 
  
  df_hts_tgt_nat <- df_hts_nat_base %>% 
    select(-results) %>% 
    pivot_wider(names_from = indicator, values_from = targets) %>% 
    mutate(linkage = TX_NEW / HTS_TST_POS)
  
  df_linkage_nat <- df_hts_nat_base %>% 
    select(-targets) %>% 
    pivot_wider(names_from = indicator, values_from = results) %>% 
    mutate(linkage = TX_NEW / HTS_TST_POS, 
           psnu = "National")
  
  # PSNU
  df_hts_psnu_base <- df_msd %>% 
    filter(indicator %in% c("HTS_TST_POS", "TX_NEW", "HTS_TST"), 
           standardizeddisaggregate == "Total Numerator",
           fiscal_year == metadata$curr_fy,
           funding_agency == "USAID") %>% 
    group_by(indicator, psnu, fiscal_year) %>% 
    summarise(across(targets:qtr4, \(x) sum(x, na.rm = T)), 
                     .groups = "drop") %>% 
    reshape_msd(direction ="semi-wide") %>% 
    group_by(indicator) %>% 
    fill(targets, .direction = "down") %>% 
    filter(nchar(period) != 4, 
           period == metadata$curr_pd) 
  
  df_hts_tgt_psnu <- df_hts_psnu_base %>% 
    select(-results) %>% 
    pivot_wider(names_from = indicator, values_from = targets) %>% 
    mutate(linkage = TX_NEW / HTS_TST_POS)
  
  df_linkage_psnu <- df_hts_psnu_base %>% 
    select(-targets) %>% 
    pivot_wider(names_from = indicator, values_from = results) %>% 
    mutate(linkage = TX_NEW / HTS_TST_POS)

  
  # VIZ ===========================================================================
  
  # Linkage visual w/ targets
  nat <- df_linkage_nat %>% 
    ggplot(aes(x = reorder(psnu, linkage))) +
    # geom_col(data = df_hts_tgt, aes(y = HTS_TST_POS), fill = "#efe9ed",
    #          position = position_nudge(x = 0.15), width = 0.5) +
    # geom_col(data = df_hts_tgt, aes(y = TX_NEW), fill = "#f8ead9", 
    #          position = position_nudge(x = -0.15), width = 0.5) +
    geom_col(aes(y = linkage), fill = scooter_light,
             position = position_nudge(x = 0.1), width = 0.5) +
    geom_text(aes(y = linkage, label = percent(linkage, 1)), 
              size = 9/.pt,
              family = "Source Sans Pro",
              fontface = "bold", 
              color = scooter, 
              vjust = 0) +
    si_style_ygrid() +
    coord_flip() +
    scale_y_continuous(labels = comma) +
    labs(x = NULL, y = NULL, 
         title = "Linkage across PSNUs",
         subtitle = glue("Linkage | {metadata$curr_pd_lab}")) +
expand_limits(x = c(0, 9)) +
    theme(
      legend.position = "none",
      plot.title = element_markdown(),
      strip.text = element_markdown(), 
      axis.text.x = element_blank())
  
  psnu <- df_linkage_psnu %>%
  ggplot(aes(x = reorder(psnu, linkage))) +
    # geom_col(data = df_hts_tgt, aes(y = HTS_TST_POS), fill = "#efe9ed",
    #          position = position_nudge(x = 0.15), width = 0.5) +
    # geom_col(data = df_hts_tgt, aes(y = TX_NEW), fill = "#f8ead9", 
    #          position = position_nudge(x = -0.15), width = 0.5) +
    geom_col(aes(y = linkage), fill = scooter_light,
             position = position_nudge(x = 0.1), width = 0.5) +
    geom_text(aes(y = linkage, label = percent(linkage, 1)), 
              size = 9/.pt,
              family = "Source Sans Pro",
              fontface = "bold", 
              color = scooter, 
              vjust = 0) +
    si_style_ygrid() +
    coord_flip() +
    scale_y_continuous(labels = comma) +
    labs(x = NULL, y = NULL, 
         caption = glue("Source: {metadata$curr_pd} MSD | Ref id: {ref_id} | US Agency for International Development")) +
    expand_limits(x = c(0, 9)) 
  
  nat/ psnu +
    plot_layout(heights = c(1, 4))
  
  si_save("Images/Linkage_summary.png")  