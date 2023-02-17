# PROJECT:  hardapoart
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  OVC - target v budget trends
# REF ID:   15e7b94c 
# LICENSE:  MIT
# DATE:     2023-02-17
# UPDATED: 


# MUNGE -------------------------------------------------------------------

prep_ovc_budget <- function(df_mer, df_fin, cntry){
  
  #clean exit if no data
  if(cntry %ni% unique(df_mer$country))
    return(NULL)
  
  #limit data to OVC non-DREAMS (and remove PEPFAR comparing agency)
  df_ovc <- df_msd %>% 
    dplyr::filter(indicator == "OVC_SERV",
                  stringr::str_detect(standardizeddisaggregate, "Age/Sex/(?!DREAMS)"),
                  country == cntry,
                  funding_agency != "PEPFAR")
  
  if(nrow(df_ovc) == 0)
    return(NULL)
  
  #limit agencies and aggregate targets
  df_ovc <- df_ovc %>% 
    dplyr::mutate(funding_agency = ifelse(funding_agency %in% c("USAID", "CDC"), funding_agency, "Other Agencies")) %>%
    dplyr::count(fiscal_year, country, funding_agency, wt = targets, name = "value") %>% 
    dplyr::mutate(type = "Targets")
  
  #filter FSD for COP budgets to OVC beneficiaries
  df_budget <- df_fsd %>% 
    filter(beneficiary == "OVC",
           country == cntry,
           fiscal_year > 2021) %>% 
    dplyr::mutate(funding_agency = ifelse(funding_agency %in% c("USAID", "CDC"), funding_agency, "Other Agencies")) %>% 
    dplyr::count(fiscal_year, country, funding_agency, wt = cop_budget_total, name = "value") %>% 
    dplyr::mutate(type = "Budget")
  
  #bind data
  df_ovc <- bind_rows(df_ovc, df_budget)
  
  #add labels for slope chart
  df_ovc <- df_ovc %>% 
    mutate(lab_min = dplyr::case_when(type == "Budget" & fiscal_year == min(fiscal_year) ~ scales::label_number(.1, prefix = "$", scale_cut = cut_short_scale())(value),
                                      fiscal_year == min(fiscal_year) ~ scales::label_number(1, scale_cut = cut_short_scale())(value)),
           lab_max = dplyr::case_when(type == "Budget" & fiscal_year == max(fiscal_year) ~ scales::label_number(.1, prefix = "$", scale_cut = cut_short_scale())(value),
                                      fiscal_year == max(fiscal_year) ~ scales::label_number(1, scale_cut = cut_short_scale())(value)))
  
  return(df_ovc)
}
  

# VIZ ---------------------------------------------------------------------

viz_ovc_budget <- function(df){
  
  if(is.null(df) || nrow(df) == 0)
    return(print(paste("No data available.")))
  
  ref_id <- "15e7b94c" #id for adorning to plots, making it easier to find on GH
  vrsn <- 1
  
  #rate of change for budget and targets
  df_gr <- df %>% 
    dplyr::group_by(funding_agency, type) %>% 
    dplyr::mutate(growth_rate = ((value / lag(value, order_by = fiscal_year))-1) %>%  scales::percent(1),
                  fiscal_year = fiscal_year - .5,
                  value_plot = (value + lag(value)) / 2) %>% 
    ungroup() %>% 
    dplyr::filter(!is.na(growth_rate))
  
  #information to plug into title
  df_title <- df_gr %>% 
    filter(funding_agency == "USAID") %>% 
    select(funding_agency, country, type, growth_rate) %>% 
    pivot_wider(names_from = type, 
                values_from = growth_rate)
  
  #plot
  df %>% 
    ggplot2::ggplot(aes(fiscal_year, value, fill = funding_agency, color = funding_agency, group = type)) +
    ggplot2::geom_area(alpha = .2) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_text(aes(label = lab_min),
                       family = "Source Sans Pro", color = glitr::matterhorn, size = 10/.pt,
                       hjust = 1.2, na.rm = TRUE) +
    ggplot2::geom_text(aes(label = lab_max),
                       family = "Source Sans Pro", color = glitr::matterhorn, size = 10/.pt,
                       hjust = -.2, na.rm = TRUE) +
    ggplot2::geom_text(data = df_gr,
                       aes(fiscal_year, value_plot, label = growth_rate),
                       family = "Source Sans Pro", color = glitr::matterhorn, size = 10/.pt,
                       vjust = -.5, na.rm = TRUE) +
    ggplot2::facet_grid(forcats::fct_rev(type) ~ forcats::fct_reorder2(funding_agency, fiscal_year, value), 
                        scales = "free_y", switch = "y") +
    ggplot2::scale_y_continuous(label = scales::label_number(scale_cut = cut_short_scale())) +
    ggplot2::scale_x_continuous(breaks = unique(df$fiscal_year)) +
    ggplot2::scale_fill_manual(values = c("USAID" = glitr::denim,
                                          "CDC" = glitr::scooter_light,
                                          "Other Agencies" = glitr::trolley_grey),
                               aesthetics = c("fill", "color")) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(x = NULL, y = NULL,
                  subtitle = glue("{df_title$funding_agency}/{df_title$country} had a {df_title$Targets} OVC target {ifelse({df_title$Targets} <0, 'decline', 'growth')} with a OVC beneficiary budget {ifelse({df_title$Budget} <0, 'decline', 'growth')} of {df_title$Budget}"),
                  caption = glue("Note: DREAMS targets removed from OVC targets; M&O and supply chain excluded from budget
                        {metadata_msd$caption} + {metadata_fsd$source} | USAID/OHA/SIEI |  Ref id: {ref_id} v{vrsn}")) +
    si_style_nolines() +
    ggplot2::theme(panel.spacing.y = ggplot2::unit(.5, "picas"),
                   axis.text.y = ggplot2::element_blank(),
                   strip.placement = "outside",
                   strip.text = ggplot2::element_text(hjust = .5),
                   legend.position = "none")
  
}
  
    
  