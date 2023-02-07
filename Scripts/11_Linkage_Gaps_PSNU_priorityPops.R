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
  ou <- "South Sudan"
  path <- "Genie_PSNU_IM_South_Sudan_Daily_2022-11-15"

# IMPORT ----------------------------------------------------------------------
  
  df <- si_path() %>%
    return_latest(path) %>%
    read_psd()
  
  load_secrets()
  get_metadata()

# MUNGE -----------------------------------------------------------------------
  
  df_msd <- df  %>% 
    filter(operatingunit == ou) %>%
    clean_agency() %>%
    resolve_knownissues()
  
  # Linkage Trends
  df_hts_base <- df_msd %>% 
    filter(indicator %in% c("HTS_TST_POS", "TX_NEW", "HTS_TST"), 
           standardizeddisaggregate == "Total Numerator",
           fiscal_year <= metadata$curr_fy, 
           funding_agency == "USAID") %>% 
    group_by(indicator, fiscal_year) %>% 
    summarise(across(targets:qtr4, \(x) sum(x, na.rm = T)), 
                     .groups = "drop") %>% 
    reshape_msd(direction ="semi-wide") %>% 
    group_by(indicator) %>% 
    fill(targets, .direction = "down") %>% 
    filter(nchar(period) != 4) 
  
  df_hts_tgt <- df_hts_base %>% 
    select(-results) %>% 
    pivot_wider(names_from = indicator, values_from = targets) %>% 
    mutate(linkage = TX_NEW / HTS_TST_POS)
  
  df_linkage <- df_hts_base %>% 
    select(-targets) %>% 
    pivot_wider(names_from = indicator, values_from = results) %>% 
    mutate(linkage = TX_NEW / HTS_TST_POS)
  
  # VIZ ===========================================================================
  
  # Linkage visual w/ targets
  bottom_hts <- df_linkage %>% 
    ggplot(aes(x = period)) +
    # geom_col(data = df_hts_tgt, aes(y = HTS_TST_POS), fill = "#efe9ed",
    #          position = position_nudge(x = 0.15), width = 0.5) +
    # geom_col(data = df_hts_tgt, aes(y = TX_NEW), fill = "#f8ead9", 
    #          position = position_nudge(x = -0.15), width = 0.5) +
    geom_col(aes(y = HTS_TST_POS), fill = "#855C75",
             position = position_nudge(x = 0.1), width = 0.5) +
    geom_col(aes(y = TX_NEW), fill = "#D9AF6B", 
             position = position_nudge(x = -0.1), width = 0.5) +
    si_style_ygrid() +
    scale_y_continuous(labels = comma) +
    labs(x = NULL, y = NULL) +
    expand_limits(x = c(0, 9)) 
  
  # Linkage plot - #b08472
  top_hts <- df_linkage %>% 
    ggplot(aes(x = period, group = 1)) +
    geom_line(aes(y = linkage), color = grey50k, size = 0.5) +
    geom_point(aes(y = linkage), shape = 19, color = "#b08472",  size = 3) + 
    geom_point(aes(y = linkage), shape = 1, color = grey90k,  size = 3) + 
    geom_text(aes(y = linkage, label = percent(linkage, 1)), 
              size = 9/.pt,
              family = "Source Sans Pro",
              fontface = "bold", 
              color = "#b08472", 
              vjust = -1.5) +
    si_style_nolines() +
    expand_limits(y = c(.85, 1), x = c(0, 9)) +
    theme(axis.text.y = element_blank(), 
          axis.text.x = element_blank()) +
    labs(x = NULL, y = NULL) +
    annotate("text", x = 8.5, y = 0.87, label = "Linkage", 
             linewidth = 11/.pt, color = "#b08472")
  
  top_hts / bottom_hts +
    plot_layout(heights = c(1, 4))
  
  
  si_save("Graphics/Linkage_summary.svg")  
  