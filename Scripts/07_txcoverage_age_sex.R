# PROJECT:  hardapoart
# AUTHOR:   K. Srikanth | USAID
# REF ID:   725ebd70
# PURPOSE:  coverage gap
# LICENSE:  MIT
# DATE:     2023-02-08
# UPDATED: adapted from groundhog_day/Scripts/FY21Q4_TXCURRSUBNAT-coverage.R
#test

 
# MUNGE -------------------------------------------------------------------

prep_txcoverage_age_sex <- function(df, cntry) {
  
  #clean exit if no data
  if(cntry %ni% unique(df$country))
    return(NULL)
  
  ind_sel <- c("PLHIV","TX_CURR_SUBNAT")
  
  df_gap <- df %>% 
    dplyr::filter(country %in% cntry,
           fiscal_year == max(fiscal_year),
           indicator %in% ind_sel,
           standardizeddisaggregate == "Age/Sex/HIVStatus",
           ageasentered != "Unknown Age") 
  
  if(nrow(df_gap) == 0)
    return(NULL)
  
  df_gap <- df_gap %>% 
    dplyr::count(fiscal_year, country, indicator, age = ageasentered, sex, wt = targets, name = "value") %>% 
    tidyr::pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}") 
  
  if("plhiv" %ni% names(df_gap))
    return(NULL)
  
  df_gap <- df_gap %>% 
    dplyr::mutate(cov_tx = tx_curr_subnat/plhiv)
  
  df_viz <- df_gap %>% 
    dplyr::mutate(plhiv_marker = dplyr::case_when(tx_curr_subnat > plhiv ~ plhiv)) %>% 
    dplyr::group_by(country) %>% 
    dplyr::mutate(facet_grp = glue::glue("{unique(df_gap$fiscal_year)} Treatment coverage gaps across {unique(df_gap$country)} <br>TX_CURR_SUBNAT ({label_number(accuracy = .1, scale_cut = cut_short_scale())(sum(tx_curr_subnat, na.rm = TRUE))}) coverage of PLHIV ({label_number(accuracy = .1, scale_cut = cut_short_scale())(sum(plhiv, na.rm = TRUE))})"),
                  lab_text = scales::percent(cov_tx, 1)) %>% 
    dplyr::ungroup()
  
  #reshape long for combining with TX_NEW plotting (and remove dup labeling in plot)
  df_viz <- df_viz %>% 
    tidyr::pivot_longer(c("plhiv", "tx_curr_subnat"),
                        names_to = "indicator") %>% 
    dplyr::mutate(indicator = toupper(indicator),
                  dplyr::across(c(cov_tx, plhiv_marker, lab_text), \(x) ifelse(indicator == "PLHIV", x, NA)))
  
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
                  standardizeddisaggregate == "Age/Sex/HIVStatus",
                  age_2019 != "Unknown Age") %>% 
    dplyr::group_by(country, fiscal_year, funding_agency, indicator, age = age_2019, sex) %>% 
    dplyr::summarise(across(starts_with("qtr"), \(x) sum(x, na.rm = TRUE)),
                     .groups = "drop") %>%
    gophr::reshape_msd(include_type = FALSE) 
  
  df_viz <- df %>% 
    dplyr::mutate(facet_grp = glue::glue("{unique(df$period)} Treatment Initiations for {unique(df$funding_agency)}/{unique(df$country)} <br> TX_NEW ({label_number(accuracy = .1, scale_cut = cut_short_scale())(sum(value, na.rm = TRUE))})")) %>%
    dplyr::mutate(lab_text = glue::glue("{clean_number(value)}"))
    
  
  return(df_viz)
  
  
}

# VIZ ---------------------------------------------------------------------


viz_txcoverage <- function(df_plhivtx, df_txnew) {
  
  df <- dplyr::bind_rows(df_plhivtx, df_txnew)
  
  q <- glue::glue("Are new patients being added to the areas with the largest gaps in TX coverage?") %>% toupper
    
  if(is.null(df) || nrow(df) == 0)
    return(dummy_plot(q))
  
  ref_id <- "725ebd70"
  vrsn <- 2
  
  df <- df %>% 
    dplyr::mutate(fill_color = dplyr::case_when(indicator == "TX_NEW" & sex == "Female" ~ glitr::moody_blue_light,
                                                sex == "Female" ~ glitr::moody_blue,
                                                indicator == "TX_NEW" ~ glitr::genoa_light,
                                                TRUE ~ glitr::genoa))
  
  df %>% 
    ggplot2::ggplot(aes(value, age, fill = fill_color, color = fill_color)) +
    ggplot2::geom_blank(aes(value*1.1), na.rm = TRUE) +
    ggplot2::geom_col(data = df %>% filter(indicator == "PLHIV"),
                      fill = NA, width = .8, alpha = .8, na.rm = TRUE) +
    ggplot2::geom_col(data = df %>% filter(indicator != "PLHIV"), 
                      width = .8, alpha = .8, na.rm = TRUE) +
    ggplot2::geom_errorbar(aes(xmin = plhiv_marker, xmax = plhiv_marker),
                           na.rm = TRUE, color = "white", linetype = "dotted") +
    ggplot2::geom_text(aes(label = lab_text), na.rm = TRUE,
                       family = "Source Sans Pro", color = glitr::suva_grey,
                       size = 10/.pt, hjust = -.5) +
    ggplot2::facet_grid(sex ~ facet_grp, switch = "y", scales = "free_x") +
    ggplot2::scale_x_continuous(label = scales::label_number(scale_cut = cut_short_scale()),
                                expand = c(.005, .005)) +
    ggplot2::scale_fill_identity(aesthetics = c("fill", "color")) +
    ggplot2::labs(x = NULL, y = NULL,
                  title = {q},
                  caption = glue("{metadata_msd$caption} + {metadata_natsubnat$source} | USAID/OHA/SIEI |  Ref id: {ref_id} v{vrsn}")) +
    ggplot2::coord_cartesian(clip = "off") +
    glitr::si_style_xgrid() +
    ggplot2::theme(strip.text.y = element_text(hjust = .5),
                   strip.text.x = element_markdown(),
                   strip.placement = "outside",
                   panel.spacing.x = unit(1, "lines"),
                   panel.spacing.y = unit(.5, "lines")
                   )
                   
}


viz_tx_all <- function(cntry, agency){
  df_cov <- prep_txcoverage_age_sex(df_natsubnat, cntry)
  df_new <- prep_txnew_age_sex(df_msd, cntry, agency)
  
  viz_txcoverage(df_cov, df_new)
}
