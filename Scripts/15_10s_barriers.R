# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  10-10-10 progress
# REF ID:   1e748b9a 
# LICENSE:  MIT
# DATE:     2023-02-07
# UPDATED: 
# SOURCE:   https://hivpolicylab.org/data
# NOTE: adapted from catch-22/Scripts/2021115_Global Planning Meeting/gpm_pepfar_10s-gap.R

# MUNGE ------------------------------------------------------------------


prep_10s_barriers <- function(df, cntry) {
  
  #select just the policy structural indicators
  ind_sel <- c(paste0("S", 1:6), "S9")
  
  #limit to just relevant indicators & rename
  df_struct <- df %>% 
    dplyr::rename(indicator = indicator_subindicator_name) %>% 
    dplyr::filter(indicator %in% ind_sel) %>% 
    dplyr::mutate(indicator_name = dplyr::recode(indicator,
                                   "S1" = "Same-sex sex non-criminalization",
                                   "S2" = "Sex work non-criminalization",
                                   "S3" = "Drug use non-criminalization",
                                   "S4" = "HIV exposure non-criminalization",
                                   "S5" = "Non-discrimination protections",
                                   "S6" = "National human rights institutions",
                                   "S9" = "Gender based violence"))
  
  #rename countries and limit to just PEPFAR
  df_struct <- df_struct %>% 
    dplyr::mutate(country = ifelse(stringr::str_detect(country, "Ivoire"), "Cote d'Ivoire", country),
           country = dplyr::recode(country,
                            "Myanmar" = "Burma",
                            "Lao People's Democratic Republic" = "Laos",
                            "Tanzania (United Republic of)" = "Tanzania",
                            "Viet Nam" = "Vietnam"
           )) %>% 
    dplyr::filter(country %in% glamr::pepfar_country_list$country)
  
  #aggregate adoption across PEPFAR countries
  df_viz <- df_struct %>% 
    dplyr::filter(year == "Most recent",
           country == cntry) %>% 
    dplyr::count(adoption_level, indicator_name) %>% 
    dplyr::filter(!is.na(adoption_level))
  
  # add viz components
  df_viz <- df_viz %>% 
    dplyr::mutate(adoption_level = factor(adoption_level, c("Adopted", "Partial", "Not adopted")),
           indicator_order = dplyr::case_when(adoption_level == "Not adopted" ~ 3,
                                       adoption_level == "Partial" ~ 2, 
                                       TRUE ~ 1),
           fill_color = dplyr::case_when(adoption_level == "Not adopted" ~ glitr::old_rose,
                                  adoption_level == "Partial" ~ glitr::burnt_sienna_light,
                                  adoption_level == "Adopted" ~ glitr::scooter_med),
           cntry = cntry) 
  
  return(df_viz)
  
}

# VIZ ---------------------------------------------------------------------


viz_10s_barriers <- function(df) {
  
  ref_id <- "1e748b9a" #for plot identification
  
  df %>% 
    ggplot2::ggplot(ggplot2::aes(n, forcats::fct_reorder(indicator_name, indicator_order, na.rm = TRUE))) +
    ggplot2::geom_col(ggplot2::aes(fill = fill_color)) +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::facet_wrap(~forcats::fct_rev(adoption_level)) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_x_continuous(position = "top") +
    ggplot2::labs(x = NULL, y = NULL,
         title = glue::glue("THE LARGEST GAP IN THE 10-10-10 GOALS IN {unique(df$cntry) %>% toupper()}"),
         subtitle = "Progress towards adopting structural laws/policies towards UNAIDS' 10-10-10 goals",
         caption = glue::glue("{metadata_pol_lab$caption}",
                        "{ref_id}", .sep = " | ")) +
    glitr::si_style_nolines() +
    ggplot2::theme(strip.placement = "outside",
          axis.text.x = ggplot2::element_blank())
  
}

