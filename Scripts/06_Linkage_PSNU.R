# PROJECT:  hardapoart
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  to visualize linkage across psnus, gaps for priority pops
# REF ID:   f6f26589 
# LICENSE:  MIT
# DATE:     2023-02-07
# UPDATED:  2023-02-10


# MUNGE -------------------------------------------------------------------


  prep_psnu_linkage <- function(df, cntry, agency, ...){
    
    #limit dataset to relevant indicators
    df_filtered <- df %>% 
      dplyr::filter(indicator %in% c("HTS_TST_POS", "TX_NEW", "HTS_TST"), 
             standardizeddisaggregate == "Total Numerator",
             fiscal_year == metadata_msd$curr_fy,
             funding_agency == agency,
             country == cntry) 
    
    #verify filter worked
    df_filtered %>% 
      assertr::verify(indicator %in% c("HTS_TST_POS", "TX_NEW", "HTS_TST") &
                        standardizeddisaggregate == "Total Numerator" &
                        fiscal_year == metadata_msd$curr_fy & 
                        funding_agency == agency &
                        country == cntry, 
                      error_fun = err_text(glue::glue("Error: {.df} has not been filtered correctly. 
                                               Please check the first filter in prep_psnu_linkage().")), 
                      description = glue::glue("Verify that the filters worked")) 
    
    #aggregate df tp psnu lvl for plotting
    df_reshaped <- df_filtered %>%
      dplyr::group_by(fiscal_year, country, funding_agency, psnu, indicator) %>% 
      dplyr::summarise(across(targets:qtr4, \(x) sum(x, na.rm = T)), 
                .groups = "drop") %>% 
      gophr::reshape_msd(direction ="semi-wide") 
    
    #verify reshape
    df_reshaped %>% 
      assertr::verify("period" %in% names(df_reshaped), 
                      error_fun = err_text(glue::glue("Error: {.df} has not been reshaped correctly and the period column does not exist. 
                                               Please check reshape_msd in prep_psnu_linkage().")), 
                      description = glue::glue("Verify that reshape_md worked"))
    
    #fill targets and remove 
    df_reshaped <- df_reshaped %>%
      dplyr::group_by(indicator) %>% 
      tidyr::fill(targets, .direction = "down") %>% 
      dplyr::filter(period == metadata_msd$curr_pd) %>% 
      dplyr::select(-targets)
    
    #verify filter to last period
    df_reshaped %>% 
      assertr::verify(period == metadata_msd$curr_pd & nchar(period) != 4, 
                      error_fun = err_text(glue::glue("Error: df_linkage_psnu has not been filtered correctly. 
                                               Please check the last filter in prep_psnu_linkage().")), 
                      description = glue::glue("Verify that last time period filtering worked")) 
    
    #create proxy linkage
    df_link <- df_reshaped %>%
      tidyr::pivot_wider(names_from = indicator, values_from = results) %>% 
      dplyr::mutate(linkage = TX_NEW / HTS_TST_POS)
    
    return(df_link)
    
  }
  
# VIZ --------------------------------------------------------------------------
  
  viz_linkage <- function(df_nat, df_psnu){
    
    
    
  # National level viz -------
    
  psnu_var_nat <- df_nat$psnu
  nat_linkage_pct <- df_nat$linkage
  
  # df_nat is national level linkage data from prep_national_linkage
  # for df_nat psnu var nat = "National"
  # linkage percent is a number from 0-1
    
  viz_link_nat <- function(df_nat, psnu_var_nat, nat_linkage_pct, ...){
    
    df_nat %>% 
      ggplot2::ggplot(aes(x = reorder(psnu_var_nat, nat_linkage_pct))) +
      ggplot2::geom_col(aes(y = nat_linkage_pct), fill = scooter_light,
                        position = position_nudge(x = 0.1), width = 0.5) +
      ggplot2::geom_text(aes(y = nat_linkage_pct, label = percent(nat_linkage_pct, 1)), 
                         size = 9/.pt,
                         family = "Source Sans Pro",
                         fontface = "bold", 
                         color = scooter, 
                         vjust = 0) +
      glitr::si_style_ygrid() +
      ggplot2::coord_flip() +
      ggplot2::scale_y_continuous(labels = comma) +
      ggplot2::labs(x = NULL, y = NULL, 
                    subtitle = glue("Linkage | {metadata_msd$curr_pd_lab}")) +
      ggplot2::expand_limits(x = c(0, 9)) +
      ggplot2::theme(
        legend.position = "none",
        plot.title = element_markdown(),
        strip.text = element_markdown(), 
        axis.text.x = element_blank())
    
  }
  
  nat <- viz_link_nat(df_nat, psnu_var_nat, nat_linkage_pct)
  
  # PSNU level viz -------
  
  psnu_var_psnu <- df_psnu$psnu
  psnu_linkage_pct <- df_psnu$linkage
  
  # df_psnu is psnu level linkage data from prep_psnu_linkage
  # for df_psnu psnu var is the actual PSNU
  # linkage percent is a number from 0-1
  
  viz_link_psnu <- function(df_psnu, psnu_var_psnu, psnu_linkage_pct){
    
    ref_id <- "f6f26589"
    
    df_psnu %>%
      ggplot(aes(x = reorder(psnu_var_psnu, psnu_linkage_pct))) +
      geom_col(aes(y = psnu_linkage_pct), fill = scooter_light,
               position = position_nudge(x = 0.1), width = 0.5) +
      geom_text(aes(y = psnu_linkage_pct, label = percent(psnu_linkage_pct, 1)), 
                size = 9/.pt,
                family = "Source Sans Pro",
                fontface = "bold", 
                color = scooter, 
                vjust = 0) +
      si_style_ygrid() +
      coord_flip() +
      scale_y_continuous(labels = comma) +
      labs(title = "Linkage, National and by PSNU",
           subtitle = glue("{metadata_msd$curr_fy_lab} | Q{metadata_msd$curr_qtr}"),
           x = NULL, y = NULL, 
           caption = glue("{metadata_msd$caption}")) +
      expand_limits(x = c(0, 9)) 
  }
  
  psnu <- viz_link_psnu(df_psnu, psnu_var_psnu, psnu_linkage_pct)
  
  # Combined viz -------
  
  # each figure could be used individually but for this visual they're combined
  nat / psnu + plot_layout(heights = c(1, 4))
  
  }