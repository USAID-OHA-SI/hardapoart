# PROJECT:  hardapoart
# AUTHOR:   K. Srikanth | USAID
# REF ID:   725ebd70
# PURPOSE:  coverage gap
# LICENSE:  MIT
# DATE:     2023-02-08
# UPDATED: adapted from groundhog_day/Scripts/FY21Q4_TXCURRSUBNAT-coverage.R

 
# MUNGE -------------------------------------------------------------------

prep_txcoverage_age_sex <- function(df, cntry) {
  
  #clean exit if no data
  if(cntry %ni% unique(df$country))
    return(NULL)
  
  ind_sel <- c("PLHIV", "DIAGNOSED_SUBNAT" ,"TX_CURR_SUBNAT", "VL_SUPPRESSION_SUBNAT")
  
  df_gap <- df %>% 
    dplyr::filter(country %in% cntry,
           fiscal_year == max(fiscal_year),
           indicator %in% ind_sel,
           standardizeddisaggregate == "Age/Sex/HIVStatus") 
  
  if(nrow(df_gap) == 0)
    return(NULL)
  
  df_gap <- df_gap %>% 
    dplyr::count(country, indicator, ageasentered, sex, wt = targets, name = "value") %>% 
    tidyr::pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}") 
  
  if("plhiv" %ni% names(df_gap))
    return(NULL)
  
  df_gap <- df_gap %>% 
    dplyr::mutate(cov_status = diagnosed_subnat/plhiv,
           cov_tx = tx_curr_subnat/plhiv)
  
  df_viz <- df_gap %>% 
    dplyr::mutate(plhiv_marker = dplyr::case_when(tx_curr_subnat > plhiv ~ plhiv),
           fill_color = ifelse(sex == "Male", glitr::genoa, glitr::moody_blue)) %>% 
    dplyr::group_by(country) %>% 
    dplyr::mutate(ctry_name = glue::glue("{unique(df_gap$country)}<br>{label_number(accuracy = .1, scale_cut = cut_short_scale())(sum(tx_curr_subnat, na.rm = TRUE))}/{label_number(accuracy = .1, scale_cut = cut_short_scale())(sum(plhiv, na.rm = TRUE))}"),
           lab_gap = scales::percent(cov_tx, 1)) %>% 
    dplyr::ungroup() %>% 
    dplyr::rename(cntry = country)
  
  return(df_viz)
  
} 

prep_txnew_age_sex <- function(df, cntry, agency) {
  
  #clean exit if no data
  if(cntry %ni% unique(df$country) | agency %ni% unique(df$funding_agency))
    return(NULL)
  
  clean_number <- function(x, digits = 0){
    dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                     x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                     x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                     TRUE ~ glue("{x}"))
  }
  
  df <- df %>% 
    dplyr::filter(country %in% cntry,
                  fiscal_year == max(fiscal_year),
                  indicator == "TX_NEW",
                  funding_agency == agency,
                  standardizeddisaggregate == "Age/Sex/HIVStatus") %>% 
    dplyr::group_by(country, fiscal_year, funding_agency, indicator, age_2019, sex) %>% 
    dplyr::summarise(across(starts_with("qtr"), sum, na.rm = TRUE),
                     .groups = "drop") %>%
    gophr::reshape_msd() 
  
  df_viz <- df %>% 
    dplyr::mutate(ctry_name = glue::glue("{unique(df$funding_agency)}/{unique(df$country)}<br>")) %>%
    dplyr::mutate(fill_color = ifelse(sex == "Male", glitr::genoa_light, glitr::moody_blue_light)) %>% 
    dplyr::rename(cntry = country) %>% 
    dplyr::group_by(cntry, period, indicator, age_2019, sex) %>% 
    dplyr::mutate(val_lab = glue::glue("{clean_number(value)}")) %>% 
    dplyr::ungroup()
    
  
  return(df_viz)
  
  
}

# VIZ ---------------------------------------------------------------------

viz_txcoverage_age_sex <- function(df) {
  
  if(is.null(df) || nrow(df) == 0)
    return(print(paste("No data available.")))
  
  ref_id <- "725ebd70"
  
  df %>% 
    ggplot2::ggplot(aes(plhiv, ageasentered, fill = fill_color, color = fill_color)) +
    ggplot2::geom_blank(aes(plhiv*1.1)) +
    ggplot2::geom_col(fill = NA, width = .8, alpha = .8) +
    ggplot2::geom_col(aes(tx_curr_subnat), width = .8, alpha = .8) +
    ggplot2::geom_errorbar(aes(xmin = plhiv_marker, xmax = plhiv_marker), 
                  na.rm = TRUE, color = "white", linetype = "dotted") +
    ggplot2::geom_text(aes(label = lab_gap), na.rm = TRUE,
              family = "Source Sans Pro", color = glitr::suva_grey,
              size = 10/.pt, hjust = -.5) +
    ggplot2::facet_grid(sex ~ forcats::fct_reorder(ctry_name, plhiv, sum, na.rm = TRUE, .desc = TRUE),
               switch = "y", scales = "free_x"
    ) +
    ggplot2::scale_x_continuous(label = scales::label_number(scale_cut = cut_short_scale()),
                       expand = c(.005, .005)) +
    ggplot2::scale_fill_identity(aesthetics = c("fill", "color")) +
    ggplot2::labs(x = NULL, y = NULL,
         title = glue::glue("{metadata_natsubnat$curr_fy_lab} {unique(df$cntry)} Treatment coverage gaps") %>% toupper,
         subtitle = "TX_CURR_SUBNAT coverage of PLHIV by age and sex",
         caption = " ") +
    ggplot2::coord_cartesian(clip = "off") +
    glitr::si_style_xgrid() +
    ggplot2::theme(strip.text.y = element_text(hjust = .5),
                  strip.text.x = element_markdown(),
                  strip.placement = "outside",
                  panel.spacing.x = unit(1, "lines"),
                  panel.spacing.y = unit(.5, "lines")
          )
  
}

viz_txnew_age_sex <- function(df) {
  
  if(is.null(df) || nrow(df) == 0)
    return(print(paste("No data available.")))
  
  ref_id <- "725ebd70"
  vrsn <- 1 
  
  df %>% 
    dplyr::filter(age_2019 != "Unknown Age",
           period == metadata_msd$curr_pd) %>% 
    ggplot2::ggplot(aes(value, age_2019, fill = fill_color, color = fill_color)) +
    ggplot2::geom_col(aes(fill = fill_color), width = .8, alpha = .8) +
    #facet_wrap(~sex, switch = "y", scales = "free_x") +
    ggplot2::facet_grid(sex ~ forcats::fct_reorder(ctry_name, value, sum, na.rm = TRUE, .desc = TRUE),
               switch = "y", scales = "free_x"
    ) +
    ggplot2:: geom_text(aes(label = val_lab), na.rm = TRUE,
              family = "Source Sans Pro", color = suva_grey,
              size = 10/.pt, hjust = -.5) +
    ggplot2::scale_fill_identity(aesthetics = c("fill", "color")) +
    ggplot2::scale_x_continuous(label = scales::label_number(scale_cut = cut_short_scale()),
                       expand = c(.005, .005)) +
    glitr::si_style_xgrid() +
    ggplot2::labs(x = NULL, y = NULL,
         title = glue::glue("{metadata_msd$curr_pd} {unique(df$funding_agency)}/{unique(df$cntry)} treatment initations") %>% toupper,
         subtitle = "TX_NEW by age and sex",
         caption = glue::glue("{metadata_msd$caption} | USAID/OHA/SIEI |  Ref Id: {ref_id} v{vrsn}")) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme(
      strip.text.y = element_blank(), #element_text(hjust = .5),
      strip.text.x = element_markdown(),
      strip.placement = "outside",
      panel.spacing.x = unit(1, "lines"),
      panel.spacing.y = unit(.5, "lines"),
      axis.text.y = element_blank())
}


viz_tx_all <- function(cntry, agency) {
  
  v1 <- prep_txcoverage_age_sex(df_natsubnat, cntry) %>% 
    viz_txcoverage_age_sex()
  
  v2 <- prep_txnew_age_sex(df_msd, cntry, agency) %>% 
    viz_txnew_age_sex()
  
  if(is.null(v1) && is.null(v2)){
    viz_tx <- NULL
    print(paste("No data available."))
  } else if(is.null(v2)){
    viz_tx <- v1
  } else if(is.null(v1)){
    viz_tx <- v2
  } else {
    suppressWarnings(
      viz_tx <- cowplot::plot_grid(v1, v2, ncol = 2, align = 'v')
    )
  }
  
  return(viz_tx)
  
  
}


#(v1 + v2) + plot_layout(widths = c(2, 1), heights = c(10))


