# PROJECT:  hardapoart
# AUTHOR:   K. Srikanth | USAID
# REF ID:   725ebd70
# PURPOSE:  coverage gap
# LICENSE:  MIT
# DATE:     2023-02-08
# UPDATED: adapted from groundhog_day/Scripts/FY21Q4_TXCURRSUBNAT-coverage.R

 
# MUNGE -------------------------------------------------------------------

prep_txcoverage_age_sex <- function(df, cntry) {
  
  ind_sel <- c("PLHIV", "DIAGNOSED_SUBNAT" ,"TX_CURR_SUBNAT", "VL_SUPPRESSION_SUBNAT")
  
  clean_number <- function(x, digits = 0){
    dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                     x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                     x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                     TRUE ~ glue("{x}"))
  }
  
  
  df_gap <- df %>% 
    dplyr::filter(country %in% cntry,
           fiscal_year == 2022,
           indicator %in% ind_sel,
           standardizeddisaggregate == "Age/Sex/HIVStatus") %>% 
    dplyr::count(country, indicator, ageasentered, sex, wt = targets, name = "value") %>% 
    tidyr::pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}") %>% 
    dplyr::mutate(cov_status = diagnosed_subnat/plhiv,
           cov_tx = tx_curr_subnat/plhiv)
  
  df_viz <- df_gap %>% 
    dplyr::mutate(plhiv_marker = dplyr::case_when(tx_curr_subnat > plhiv ~ plhiv),
           fill_color = ifelse(sex == "Male", glitr::genoa, glitr::moody_blue)) %>% 
    dplyr::group_by(country) %>% 
    dplyr::mutate(ctry_name = glue::glue("{country}<br>{label_number_si(accuracy = .1)(sum(tx_curr_subnat, na.rm = TRUE))}/{label_number_si(accuracy = .1)(sum(plhiv, na.rm = TRUE))}"),
           lab_gap = dplyr::case_when(cov_tx < .95^2 ~ scales::percent(cov_tx, 1))) %>% 
    dplyr::ungroup() %>% 
    dplyr::rename(cntry = country)
  
  return(df_viz)
  
} 

prep_txnew_age_sex <- function(df, cntry) {
  
  df_viz <- df %>% 
    dplyr::filter(country %in% cntry,
                  fiscal_year == 2022,
                  indicator == "TX_NEW",
                  standardizeddisaggregate == "Age/Sex/HIVStatus") %>% 
    group_by(country, fiscal_year, indicator, ageasentered, sex) %>% 
    dplyr::summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    gophr::reshape_msd() %>% 
    mutate(fill_color = ifelse(sex == "Male", glitr::genoa_light, glitr::moody_blue_light)) %>% 
    dplyr::rename(cntry = country)
  
  return(df_viz)
  
  
}

# VIZ ---------------------------------------------------------------------

viz_txcoverage_age_sex <- function(df) {
  
  ref_id <- "725ebd70"
  
  df %>% 
    ggplot(aes(plhiv, ageasentered, fill = fill_color, color = fill_color)) +
    geom_blank(aes(plhiv*1.1)) +
    geom_col(fill = NA, width = .8, alpha = .8) +
    geom_col(aes(tx_curr_subnat), width = .8, alpha = .8) +
    geom_errorbar(aes(xmin = plhiv_marker, xmax = plhiv_marker), 
                  na.rm = TRUE, color = "white", linetype = "dotted") +
    geom_text(aes(label = lab_gap), na.rm = TRUE,
              family = "Source Sans Pro", color = suva_grey,
              size = 10/.pt, hjust = -.5) +
    facet_grid(sex ~ fct_reorder(ctry_name, plhiv, sum, na.rm = TRUE, .desc = TRUE),
               switch = "y", scales = "free_x"
    ) +
    scale_x_continuous(labels = label_number_si(),
                       expand = c(.005, .005)) +
    scale_fill_identity(aesthetics = c("fill", "color")) +
    labs(x = NULL, y = NULL,
         title = glue("Treatment coverage gaps in {unique(df$cntry) %>% toupper()} by age and sex") %>% toupper,
         subtitle = "TX_CURR_SUBNAT coverage of PLHIV",
         caption = glue("{metadata_natsubnat$caption}")) +
    coord_cartesian(clip = "off") +
    si_style_xgrid() +
    theme(strip.text.y = element_text(hjust = .5),
          strip.text.x = element_markdown(),
          strip.placement = "outside",
          panel.spacing.x = unit(1, "lines"),
          panel.spacing.y = unit(.5, "lines")
          )
  
}

viz_txnew_age_sex <- function(df) {
  
  df %>% 
    filter(ageasentered != "Unknown Age") %>% 
    ggplot(aes(value, ageasentered, fill = fill_color, color = fill_color)) +
    geom_col(aes(fill = fill_color), width = .8, alpha = .8) +
    #facet_wrap(~sex, switch = "y", scales = "free_x") +
    facet_grid(sex ~ fct_reorder(cntry, value, sum, na.rm = TRUE, .desc = TRUE),
               switch = "y", scales = "free_x"
    ) +
    scale_fill_identity(aesthetics = c("fill", "color")) +
    si_style_xgrid() +
    labs(x = NULL, y = NULL,
         title = glue("{metadata_msd$curr_pd} treatment initations in {unique(df$cntry) %>% toupper()}") %>% toupper,
         subtitle = "TX_CURR_SUBNAT coverage of PLHIV",
         caption = glue("{metadata_msd$caption}")) +
    coord_cartesian(clip = "off") +
    theme(
      strip.text.y = element_text(hjust = .5),
          strip.text.x = element_markdown(),
          strip.placement = "outside",
          panel.spacing.x = unit(1, "lines"),
          panel.spacing.y = unit(.5, "lines"),
          axis.text.y = element_blank())
 }

v1 <- prep_txcoverage_age_sex(df_natsubnat, "Zambia") %>% 
  viz_txcoverage_age_sex()

v2 <- prep_txnew_age_sex(df_msd, "Zambia") %>% 
  viz_txnew_age_sex()

#(v1 + v2) + plot_layout(widths = c(2, 1), heights = c(10))

cowplot::plot_grid(v1, v2, ncol = 2, align = 'v')

