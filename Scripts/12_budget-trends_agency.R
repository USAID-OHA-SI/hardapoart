# PROJECT:  hardapoart
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  budget trends
# REF ID:   11a316e1 
# LICENSE:  MIT
# DATE:     2023-02-03
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(gagglr)
  library(glue)
  library(scales)
  library(extrafont)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "11a316e1" #id for adorning to plots, making it easier to find on GH
  
  get_metadata(type = "Financial") #list of MSD metadata elements
  
  load_secrets("email")
  
  cntry <- "Malawi"
  agency <- "USAID"

# IMPORT ------------------------------------------------------------------
  
  df_fsd <- si_path() %>% 
    return_latest("Fin") %>% 
    read_msd()   
  

# MUNGE -------------------------------------------------------------------
  
  
  #limit to select country
  df_fsd <- df_fsd %>% 
    filter(country == cntry,
           cop_budget_total != 0)
  
  #remove M&O and supply chain
  df_fsd <- df_fsd %>% 
    remove_mo() %>% 
    remove_sch()
  
  
  #agency and PEPFAR total
  df_fsd_agg <- df_fsd %>% 
    bind_rows(df_fsd %>% 
                mutate(funding_agency = "PEPFAR")) %>% 
    # filter(funding_agency %in% c(agency, "PEPFAR")) %>% 
    count(country, planning_cycle, fiscal_year, funding_agency, wt = cop_budget_total, name = "cop_budget_total")


  
  df_fsd_agg <- df_fsd %>% 
    clean_agency() %>% 
    mutate(funding_agency = ifelse(funding_agency %in% c("USAID", "CDC"), funding_agency, "Other Agencies")) %>% 
    count(country, planning_cycle, fiscal_year, funding_agency, wt = cop_budget_total, name = "cop_budget_total") %>% 
    group_by(fiscal_year) %>% 
    mutate(pepfar_total = sum(cop_budget_total)) %>% 
    ungroup()
  
  
  df_viz <- df_fsd_agg %>% 
    group_by(funding_agency) %>% 
    mutate(lab = case_when(cop_budget_total == max(cop_budget_total) ~ cop_budget_total,
                           fiscal_year == max(fiscal_year) ~ cop_budget_total)) %>% 
    ungroup()
  
# VIZ ---------------------------------------------------------------------

  
  df_viz %>% 
    ggplot(aes(fiscal_year, cop_budget_total, fill = funding_agency)) +
    geom_col(aes(y = pepfar_total), fill = "#909090", alpha = .2) +
    geom_col(alpha = .6) +
    geom_text(aes(label = label_number(prefix = "$",scale_cut = cut_short_scale())(lab)), 
              family = "Source Sans Pro", color = matterhorn, vjust = -.3, na.rm = TRUE) +
    facet_grid(~fct_reorder2(funding_agency, fiscal_year, cop_budget_total)) +
    scale_y_continuous(label = label_number(prefix = "$", scale_cut = cut_short_scale()),
                       expand = c(.005, .005)) +
    scale_fill_manual(values = c("USAID" = denim,
                                 "CDC" = scooter_light,
                                 "Other" = trolley_grey)) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL,
         subtitle = glue("{cntry}'s annual budget shifts by agency (fiscal year)"),
         caption = glue("Note: M&O and supply chain excluded
                        {metadata$caption}")) +
    si_style_ygrid() +
    theme(legend.position = "none")

  si_save("Images/12_budget-trends_agency.png")
  