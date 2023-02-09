# PROJECT:  hardapoart
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  to visualize linkage across psnus, gaps for priority pops
# REF ID:   f6f26589 
# LICENSE:  MIT
# DATE:     2023-02-07
# UPDATED: 

# DEPENDENCIES ----------------------------------------------------------------
  source("Scripts/91_setup.R")

  # I can't load the PSNUxIM file so I'm trusting that it works
  # .df is the PSNUxIM MSD df_msd
  # .ou = a character string
  
  prep_national_linkage <- function(.df, .ou, ...){
    
    
    df_linkage_nat <- .df %>% 
      filter(indicator %in% c("HTS_TST_POS", "TX_NEW", "HTS_TST"), 
             standardizeddisaggregate == "Total Numerator",
             fiscal_year == metadata$curr_fy,
             funding_agency == "USAID",
             operatingunit == .ou) %>% 
      group_by(indicator, fiscal_year) %>% 
      summarise(across(targets:qtr4, \(x) sum(x, na.rm = T)), 
                .groups = "drop") %>% 
      reshape_msd(direction ="semi-wide") %>% 
      group_by(indicator) %>% 
      fill(targets, .direction = "down") %>% 
      filter(nchar(period) != 4, 
             period == metadata$curr_pd) %>% 
      select(-targets) %>% 
      pivot_wider(names_from = indicator, values_from = results) %>% 
      mutate(linkage = TX_NEW / HTS_TST_POS, 
             psnu = "National")
    
    return(df_linkage_nat)
    
}
  prep_psnu_linkage <- function(.df, .ou, ...){
    
    
    df_linkage_psnu <- .df %>% 
      filter(indicator %in% c("HTS_TST_POS", "TX_NEW", "HTS_TST"), 
             standardizeddisaggregate == "Total Numerator",
             fiscal_year == metadata$curr_fy,
             funding_agency == "USAID",
             operatingunit == .ou) %>% 
      group_by(indicator, psnu, fiscal_year) %>% 
      summarise(across(targets:qtr4, \(x) sum(x, na.rm = T)), 
                .groups = "drop") %>% 
      reshape_msd(direction ="semi-wide") %>% 
      group_by(indicator) %>% 
      fill(targets, .direction = "down") %>% 
      filter(nchar(period) != 4, 
             period == metadata$curr_pd) %>% 
      select(-targets) %>% 
      pivot_wider(names_from = indicator, values_from = results) %>% 
      mutate(linkage = TX_NEW / HTS_TST_POS)
    
    return(df_linkage_psnu)
    
  }
  
  # df_nat is the national level linkage data
  # for df_nat psnu var = "National"
  # linkage percent is a number from 0-1
  # ref_id is the image reference id
  # df_psnu is the psnu level linkage data
  viz_link_nat <- function(.df_nat, .psnu_var, .linkage_pct, .ref_id, ...){
    
    .df_nat %>% 
      ggplot(aes(x = reorder(psnu, linkage))) +
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
           subtitle = glue("Linkage | {metadata$curr_pd_lab}")) +
      expand_limits(x = c(0, 9)) +
      theme(
        legend.position = "none",
        plot.title = element_markdown(),
        strip.text = element_markdown(), 
        axis.text.x = element_blank())
    
  }
  
  # for df_psnu psnu var is the actual PSNU
  # linkage percent is a number from 0-1
  # linkage percent is a number from 0-1
  # ref_id is the image reference id
  
  viz_link_psnu <- function(.df_psnu, .psnu_var, .linkage_pct, .ref_id, ...){
    
    .df_psnu %>%
      ggplot(aes(x = reorder(.psnu_var, .linkage_pct))) +
      geom_col(aes(y = .linkage_pct), fill = scooter_light,
               position = position_nudge(x = 0.1), width = 0.5) +
      geom_text(aes(y = .linkage_pct, label = percent(.linkage_pct, 1)), 
                size = 9/.pt,
                family = "Source Sans Pro",
                fontface = "bold", 
                color = scooter, 
                vjust = 0) +
      si_style_ygrid() +
      coord_flip() +
      scale_y_continuous(labels = comma) +
      labs(x = NULL, y = NULL, 
           caption = glue("Source: {metadata$curr_pd} MSD | Ref id: {.ref_id} | US Agency for International Development")) +
      expand_limits(x = c(0, 9)) 
  }

# GLOBAL VARIABLES ------------------------------------------------------------
  
  ref_id <- "f6f26589"

# MUNGE -----------------------------------------------------------------------

  link_nat <- prep_national_linkage(df_msd, "Democratic Republic of the Congo")
  link_psnu <- prep_psnu_linkage(df_msd, "Democratic Republic of the Congo")
  
  # VIZ ========================================================================

  nat <- viz_link_nat(link_nat, link_nat$psnu, link_nat$linkage, ref_id)
  psnu <- viz_link_psnu(link_psnu, link_psnu$psnu, link_psnu$linkage, ref_id)

  nat / psnu + plot_layout(heights = c(1, 4))

  si_save("Images/Linkage_summary.png")  