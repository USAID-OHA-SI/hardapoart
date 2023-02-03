# PROJECT:  hardapoart
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  optimal mix of PEPFAR spending?
# REF ID:   e258e5d3 
# LICENSE:  MIT
# DATE:     2023-02-02
# UPDATED:  2023-02-03

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
  
  ref_id <- "e258e5d3" #id for adorning to plots, making it easier to find on GH
  
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
           expenditure_amt != 0)
  
  #assign funding type
  df_fsd <- df_fsd %>% 
    mutate(funding_type = case_when(interaction_type == "Service Delivery" ~ "Service Delivery (SD)",
                                    interaction_type == "Not Specified" ~ interaction_type,
                                    program == "ASP" ~ "Above Site Non-SD",
                                    program %in% c("PM", "Applied Pipeline") ~ program,
                                    interaction_type == "Non Service Delivery" ~ "Site Level Non-SD")) 
  
  #remove M&O and supply chain
  df_fsd <- df_fsd %>% 
    remove_mo() %>% 
    remove_sch()
  
  #aggregate by year and 
  df_fsd_agg <- df_fsd %>% 
    filter(funding_agency == agency) %>% 
    count(country, fiscal_year, funding_agency, funding_type, wt = expenditure_amt, name = "exp_amt") %>% 
    group_by(fiscal_year) %>% 
    mutate(exp_total = sum(exp_amt, na.rm = TRUE)) %>% 
    ungroup() 
  
  #labels for each type's share of the total
  df_viz <- df_fsd_agg %>% 
    mutate(lab = case_when(fiscal_year == max(fiscal_year) ~ percent(exp_amt/exp_total, 1)))
  

# VIZ ---------------------------------------------------------------------

  
  df_viz %>% 
    ggplot(aes(fiscal_year, exp_amt, group = funding_type, fill = funding_type)) +
    geom_area(aes(y = exp_total), fill = "#909090", alpha = .2) +
    geom_area(alpha = .8) +
    geom_text(aes(label = lab), family = "Source Sans Pro", hjust = -.1,
              color = matterhorn, na.rm = TRUE) +
    facet_grid(~fct_reorder2(funding_type, fiscal_year, exp_amt)) +
    scale_y_continuous(label = number_format(scale = 1e-6, prefix = "$", suffix = "M"),
                       expand = c(.005, .005)) + 
    scale_fill_si("genoa", discrete = TRUE) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL, fill = NA,
         subtitle = glue("{unique(df_viz$funding_agency)}/{unique(df_viz$country)}'s breakdown of annual expenditures by funding type"),
         caption = glue("Note: M&O and supply chain excluded
                        {metadata$caption}")) +
    si_style_ygrid() +
    theme(legend.position = "none")

  si_save("Images/13_funding-distro_funding-flavors.png")
  