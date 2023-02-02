# PROJECT:  hardapoart
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  optimal mix of PEPFAR spending?
# REF ID:   e258e5d3 
# LICENSE:  MIT
# DATE:     2023-02-02
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
  
  ref_id <- "e258e5d3" #id for adorning to plots, making it easier to find on GH
  
  get_metadata(type = "Financial") #list of MSD metadata elements

  load_secrets("email")
  
  cntry <- "Malawi"
  
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
    mutate(funding_type = case_when(interaction_type %in% c("Not Specified", "Service Delivery") ~ interaction_type,
                                    program == "ASP" ~ "Above Site NSD",
                                    program %in% c("PM", "Applied Pipeline") ~ program,
                                    interaction_type == "Non Service Delivery" ~ "Site Level NSD")) 
  
  #remove M&O and supply chain
  df_fsd <- df_fsd %>% 
    remove_mo() %>% 
    remove_sch()
  
  #aggregate by year and 
  df_fsd_agg <- df_fsd %>% 
    mutate(agency = ifelse(funding_agency == "USAID", "USAID", "Other")) %>% 
    count(fiscal_year, agency, funding_type, wt = expenditure_amt, name = "expenditure_amt") %>% 
    pivot_wider(names_from = agency, values_from = expenditure_amt,
                names_glue = "{tolower(agency)}") %>% 
    rowwise() %>% 
    mutate(pepfar = sum(usaid, other, na.rm = TRUE)) %>% 
    ungroup() %>% 
    group_by(fiscal_year) %>% 
    mutate(usaid_total = sum(usaid, na.rm = TRUE)) %>% 
    ungroup()
  

# VIZ ---------------------------------------------------------------------

  
  df_fsd_agg %>% 
    ggplot(aes(fiscal_year, usaid, group = funding_type, fill = funding_type)) +
    geom_area(aes(y = usaid_total), fill = "#909090", alpha = .2) +
    geom_area() +
    facet_grid(~fct_reorder2(funding_type, fiscal_year, usaid)) +
    scale_y_continuous(label = number_format(scale = 1e-6, prefix = "$", suffix = "M")) + 
    labs(x = NULL, y = NULL,
         caption = glue("{metadata$caption}")) +
    si_style_ygrid()
