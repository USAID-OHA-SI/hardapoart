# PROJECT:  hardapoart
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  to visualize linkage across psnus, gaps for priority pops
# REF ID:   f6f26589 
# LICENSE:  MIT
# DATE:     2023-02-07
# UPDATED:  2023-02-10

# DEPENDENCIES -----------------------------------------------------------------
  source("Scripts/91_setup.R")

  # .df is the PSNUxIM MSD df_msd
  # .ou = a character string in pepfar_country_list$operatingunit
  prep_national_linkage <- function(df, ou, ...){
  
    df_reshaped <- df %>% 
      dplyr::filter(indicator %in% c("HTS_TST_POS", "TX_NEW", "HTS_TST"), 
             standardizeddisaggregate == "Total Numerator",
             fiscal_year == metadata$curr_fy,
             funding_agency == "USAID",
             operatingunit == ou) %>% 
      assertr::verify(indicator %in% c("HTS_TST_POS", "TX_NEW", "HTS_TST") &
               standardizeddisaggregate == "Total Numerator" &
               fiscal_year == metadata$curr_fy & 
               funding_agency == "USAID" &
               operatingunit == ou, 
               error_fun = err_text(glue::glue("Error: {.df} has not been filtered correctly. 
                                               Please check the first filter in prep_national_linkage().")), 
               description = glue::glue("Verify that the filters worked")) %>%
      dplyr::group_by(indicator, fiscal_year) %>% 
      dplyr::summarise(across(targets:qtr4, \(x) sum(x, na.rm = T)), 
                .groups = "drop") %>% 
      gophr::reshape_msd(direction ="semi-wide") 
    
    df_linkage_nat  <- df_reshaped %>% 
      assertr::verify("period" %in% names(df_reshaped), 
                      error_fun = err_text(glue::glue("Error: {.df} has not been reshaped correctly and the period column does not exist. 
                                               Please check reshape_msd in prep_national_linkage().")), 
                      description = glue::glue("Verify that reshape_md worked")) %>%
      dplyr::group_by(indicator) %>% 
      tidyr::fill(targets, .direction = "down") %>% 
      dplyr::filter(nchar(period) != 4, 
             period == metadata$curr_pd) %>% 
      assertr::verify(period == metadata$curr_pd & nchar(period) != 4, 
                      error_fun = err_text(glue::glue("Error: df_linkage_nat has not been filtered correctly. 
                                               Please check the last filter in prep_national_linkage().")), 
                      description = glue::glue("Verify that last time period filtering worked")) %>%
      dplyr::select(-targets) %>% 
      tidyr::pivot_wider(names_from = indicator, values_from = results) %>% 
      dplyr::mutate(linkage = TX_NEW / HTS_TST_POS, 
             psnu = "National") %>%
    assertr::verify(psnu == "National", 
                    error_fun = err_text(glue::glue("Error: PSNU in df_linkage_nat has not been assigned correctly. 
                                               Please check the last mutate() in prep_national_linkage().")), 
                    description = glue::glue("Verify that PSNU is National for this dataset"))
    
    return(df_linkage_nat)
    
}
  prep_psnu_linkage <- function(df, ou, ...){
    
    
    df_reshaped <- df %>% 
      dplyr::filter(indicator %in% c("HTS_TST_POS", "TX_NEW", "HTS_TST"), 
             standardizeddisaggregate == "Total Numerator",
             fiscal_year == metadata$curr_fy,
             funding_agency == "USAID",
             operatingunit == ou) %>% 
      assertr::verify(indicator %in% c("HTS_TST_POS", "TX_NEW", "HTS_TST") &
                        standardizeddisaggregate == "Total Numerator" &
                        fiscal_year == metadata$curr_fy & 
                        funding_agency == "USAID" &
                        operatingunit == ou, 
                      error_fun = err_text(glue::glue("Error: {.df} has not been filtered correctly. 
                                               Please check the first filter in prep_psnu_linkage().")), 
                      description = glue::glue("Verify that the filters worked")) %>%
      dplyr::group_by(indicator, psnu, fiscal_year) %>% 
      dplyr::summarise(across(targets:qtr4, \(x) sum(x, na.rm = T)), 
                .groups = "drop") %>% 
      gophr::reshape_msd(direction ="semi-wide") 
    
    df_linkage_psnu <- df_reshaped %>% 
      assertr::verify("period" %in% names(df_reshaped), 
                      error_fun = err_text(glue::glue("Error: {.df} has not been reshaped correctly and the period column does not exist. 
                                               Please check reshape_msd in prep_psnu_linkage().")), 
                      description = glue::glue("Verify that reshape_md worked")) %>%
      dplyr::group_by(indicator) %>% 
      tidyr::fill(targets, .direction = "down") %>% 
      dplyr::filter(nchar(period) != 4, 
             period == metadata$curr_pd) %>% 
      assertr::verify(period == metadata$curr_pd & nchar(period) != 4, 
                      error_fun = err_text(glue::glue("Error: df_linkage_psnu has not been filtered correctly. 
                                               Please check the last filter in prep_psnu_linkage().")), 
                      description = glue::glue("Verify that last time period filtering worked")) %>%
      dplyr::select(-targets) %>% 
      tidyr::pivot_wider(names_from = indicator, values_from = results) %>% 
      dplyr::mutate(linkage = TX_NEW / HTS_TST_POS) %>%
      assertr::verify(psnu != "National" & is.na(psnu) == FALSE,
      error_fun = err_text(glue::glue("Error: PSNU in df_linkage_psnu has not been assigned correctly. 
                                      Please check the values of psnu in df_reshaped in prep_psnu_linkage().")), 
                           description = glue::glue("Verify that PSNU is not National or missing for this dataset"))
    
    return(df_linkage_psnu)
    
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
                    subtitle = glue("Linkage | {metadata$curr_pd_lab}")) +
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
      labs(x = NULL, y = NULL, 
           caption = glue("Source: {metadata$curr_pd} MSD | Ref id: {ref_id} | US Agency for International Development")) +
      expand_limits(x = c(0, 9)) 
  }
  
  psnu <- viz_link_psnu(df_psnu, psnu_var_psnu, psnu_linkage_pct)
  
  # Combined viz -------
  
  # each figure could be used individually but for this visual they're combined
  nat / psnu + plot_layout(heights = c(1, 4))
  
  }