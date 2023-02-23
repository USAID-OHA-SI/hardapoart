# PROJECT:  hardapoart
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  ovc treatment coverage
# REF ID:   7f4c9dc0 
# LICENSE:  MIT
# DATE:     2023-02-16
# UPDATED:  2023-02-22


# MUNGE -------------------------------------------------------------------

  
prep_ovc_coverage <- function(df_mer, df_subnat, cntry, agency){
  
  #under 15 fine ages
  u15 <- c("<01", "01-04", "05-09", "10-14") #"15-17"
  
  #limit PLHIV to <15
  df_clhiv <- df_natsubnat %>% 
    dplyr::filter(indicator == "PLHIV",
                  ageasentered %in% u15,
                  standardizeddisaggregate == "Age/Sex/HIVStatus",
                  country == cntry) %>% 
    count(fiscal_year, country, psnu, wt = targets, name = "clhiv")
  
  #limit to indicator needed + country/agency
  df_ovctx <- df_msd %>% 
    dplyr::filter(
      (indicator == "TX_CURR" & standardizeddisaggregate == "Age/Sex/HIVStatus" & ageasentered %in% u15 ) |
        (indicator == "OVC_HIVSTAT_POS" & otherdisaggregate == "Receiving ART" & ageasentered %in% u15) |
        (indicator == "OVC_HIVSTAT" & standardizeddisaggregate == "Total Numerator"),
      country == cntry,
      funding_agency == agency) %>% 
    mutate(indicator = ifelse(indicator == "OVC_HIVSTAT_POS", "OVC_HIVSTAT_ART", indicator))
  
  if(nrow(df_ovctx) == 0)
    return(NULL)
  
  #aggregate totals for each PSNU
  df_ovctx <- df_ovctx %>% 
    dplyr::group_by(fiscal_year, country, psnu, funding_agency, indicator) %>% 
    dplyr::summarise(dplyr::across(dplyr::starts_with("qtr"), \(x) sum(x, na.rm = TRUE)),
                     .groups = "drop")
  
  #rehsape long by period (to take max) and wide by indicator to id all the OVC_HIVSTAT PSNUs
  df_ovctx <- df_ovctx %>% 
    dplyr::mutate(fiscal_year_orig = fiscal_year, .before = 1) %>% 
    gophr::reshape_msd(include_type = FALSE) %>% 
    dplyr::rename(fiscal_year = fiscal_year_orig) %>% 
    dplyr::filter(value != 0) %>% 
    tidyr::pivot_wider(names_from = indicator,
                       names_glue = "{tolower(indicator)}")
  
  if("ovc_hivstat" %ni% names(df_ovctx))
    return(NULL)
  
  #keep only OVC PSNUs and only keep last period
  df_ovctx <- df_ovctx %>%  
    dplyr::filter(!is.na(ovc_hivstat)) %>% 
    dplyr::filter(period == max(period))
  
  #join with clhiv
  df_ovctx <- df_ovctx %>% 
    dplyr::left_join(df_clhiv, by = join_by(fiscal_year, country, psnu)) 
  
  #create variable in the event they are missing
  if(!"clhiv" %in% names(df_ovctx))
    df_ovctx <- dplyr::mutate(df_ovctx, clhiv = NA)
  if(!"ovc_hivstat_art" %in% names(df_ovctx))
    df_ovctx <- dplyr::mutate(df_ovctx, ovc_hivstat_art = NA)
  if(!"tx_curr" %in% names(df_ovctx))
    df_ovctx <- dplyr::mutate(df_ovctx, tx_curr = NA)
  
  #expected coverage
  cov_rate <- .9
  
  #calculate coverage
  df_ovctx <- df_ovctx %>% 
    dplyr::mutate(coverage_clhiv = ovc_hivstat_art / clhiv,
                  coverage_tx = ovc_hivstat_art / tx_curr,
                  group = ifelse(coverage_tx <=cov_rate, glue("Below {percent(cov_rate)} Coverage"), ""),
                  fill_alpha = ifelse(coverage_tx <=cov_rate, .8, .5))
  
  return(df_ovctx)
}
  
  

# VIZ ---------------------------------------------------------------------


viz_ovc_coverage <- function(df){
  
  q <- glue::glue("What proportion of PEPFAR's <15 TX cohort is the OVC Comprehensive program reaching?") %>% toupper
    
  if(is.null(df) || nrow(df) == 0)
    return(dummy_plot(q))
  
  ref_id <- "7f4c9dc0" #id for adorning to plots, making it easier to find on GH
  vrsn <- 2 
  
  #overall coverage to include in plot title
  overall_cov <- sum(df$ovc_hivstat_art, na.rm = TRUE) / sum(df$tx_curr)
  
  #limit to 21 bars (overall + 20 psnus)
  cap_note <- ifelse(nrow(df) > 20, "Note: Limited to the largest 20 TX_CURR <15 PSNUs\n", "")
  df <- df %>% 
    dplyr::slice_max(order_by = tx_curr, n = 20)
  
  df %>%
    ggplot2::ggplot(aes(y = forcats::fct_reorder(psnu, tx_curr, na.rm = TRUE), alpha = fill_alpha)) +
    ggplot2::geom_col(aes(tx_curr), color = glitr::moody_blue, fill = NA, na.rm = TRUE) +
    ggplot2::geom_col(aes(ovc_hivstat_art), fill = glitr::moody_blue, na.rm = TRUE) +
    ggplot2::geom_text(aes(x = 0, label = scales::percent(coverage_tx, 1)),
                       hjust = -.5, family = "Source Sans Pro SemiBold", color = "white", na.rm = TRUE) +
    ggplot2::facet_grid(forcats::fct_rev(group) ~ ., scale = "free_y", space = "free", switch = "y") +
    ggplot2::scale_x_continuous(label = scales::comma, expand = c(.005, .005)) +
    ggplot2::scale_alpha_identity() +
    ggplot2::labs(x = NULL, y = NULL,
                  title = {q},
                  subtitle = glue::glue("{unique(df$period)} {unique(df$funding_agency)}/{unique(df$country)} **<span style = 'color:{glitr::moody_blue};'>comprehensive OVC program </span>** covered {scales::percent(overall_cov,1)} of <15 on Treatment in PSNUs with OVC Programming"),
                  caption = glue::glue("{cap_note}{metadata_msd$caption} | USAID/OHA/SIEI |  Ref id: {ref_id} v{vrsn}")) +
    glitr::si_style_xgrid() +
    ggplot2::theme(strip.placement = "outside",
                   panel.spacing = ggplot2::unit(.5, "picas"),
                   strip.text.y = ggplot2::element_text(hjust = .5),
                   plot.subtitle = ggtext::element_markdown())
  
  
}  

