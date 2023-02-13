# PROJECT:  hardapoart
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  budget trends
# REF ID:   11a316e1 
# LICENSE:  MIT
# DATE:     2023-02-03
# UPDATED: 


# MUNGE -------------------------------------------------------------------
  
prep_budget_trends <- function(df, cntry){
  
  if(cntry %ni% unique(df$country))
    return(NULL)
  
  #limit to select country
  df_int <- df %>% 
    dplyr::filter(country == cntry,
                  cop_budget_total != 0)
  
  #remove M&O and supply chain
  df_int <- df_int %>% 
    gophr::remove_mo() %>% 
    gophr::remove_sch()
  
  #agency and PEPFAR total
  df_int_agg <- df_int %>% 
    gophr::clean_agency() %>% 
    dplyr::mutate(funding_agency = ifelse(funding_agency %in% c("USAID", "CDC"), funding_agency, "Other Agencies")) %>% 
    dplyr::count(country, planning_cycle, fiscal_year, funding_agency, wt = cop_budget_total, name = "cop_budget_total") %>% 
    dplyr::group_by(fiscal_year) %>% 
    dplyr::mutate(pepfar_total = sum(cop_budget_total)) %>% 
    dplyr::ungroup()
  
  #format for viz
  df_int_agg <- df_int_agg %>% 
    dplyr::group_by(funding_agency) %>% 
    dplyr::mutate(lab = dplyr::case_when(cop_budget_total == max(cop_budget_total) ~ cop_budget_total,
                           fiscal_year == max(fiscal_year) ~ cop_budget_total)) %>% 
    dplyr::ungroup()
  
  return(df_int_agg)
  
}
  
# VIZ ---------------------------------------------------------------------


viz_budget_trends <- function(df){
  
  if(is.null(df) || nrow(df) == 0)
    return(print(paste("No data available.")))
  
  ref_id <- "11a316e1"
  vrsn <- 1 
  
  df %>% 
    ggplot2::ggplot(aes(fiscal_year, cop_budget_total, fill = funding_agency)) +
    ggplot2::geom_col(aes(y = pepfar_total), fill = "#909090", alpha = .2) +
    ggplot2::geom_col(alpha = .6) +
    ggplot2::geom_text(aes(label = scales::label_number(prefix = "$",scale_cut = cut_short_scale())(lab)), 
              family = "Source Sans Pro", color = glitr::matterhorn, vjust = -.3, na.rm = TRUE) +
    ggplot2::facet_grid(~forcats::fct_reorder2(funding_agency, fiscal_year, cop_budget_total)) +
    ggplot2::scale_y_continuous(label = scales::label_number(prefix = "$", scale_cut = cut_short_scale()),
                       expand = c(.005, .005)) +
    ggplot2::scale_fill_manual(values = c("USAID" = glitr::denim,
                                 "CDC" = glitr::scooter_light,
                                 "Other" = glitr::trolley_grey)) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(x = NULL, y = NULL,
         subtitle = glue("{unique(df$country)}'s annual budget shifts by agency (fiscal year)"),
         caption = glue("Note: M&O and supply chain excluded
                        {metadata_fsd$caption} | USAID | Ref id: {ref_id} v{vrsn}")) +
    si_style_ygrid() +
    ggplot2::theme(legend.position = "none")
}
  
  