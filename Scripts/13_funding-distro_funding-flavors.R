# PROJECT:  hardapoart
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  optimal mix of PEPFAR spending?
# REF ID:   e258e5d3 
# LICENSE:  MIT
# DATE:     2023-02-02
# UPDATED:  2023-02-03


# MUNGE -------------------------------------------------------------------

prep_funding_distro <- function(df, cntry, agency){
  
  #limit to select country
  df_int <- df %>% 
    dplyr::filter(country == cntry,
                  expenditure_amt != 0)
  
  #assign funding type
  df_int <- gophr::apply_funding_type(df_int) 
  
  #remove M&O and supply chain
  df_int <- df_int %>% 
    gophr::remove_mo() %>% 
    gophr::remove_sch()
  
  #aggregate by year and 
  df_int_agg <- df_int %>% 
    dplyr::filter(funding_agency == agency) %>% 
    dplyr::count(country, fiscal_year, funding_agency, funding_type, 
                 wt = expenditure_amt, name = "exp_amt") %>% 
    dplyr::group_by(fiscal_year) %>% 
    dplyr::mutate(exp_total = sum(exp_amt, na.rm = TRUE)) %>% 
    dplyr::ungroup() 
  
  #labels for each type's share of the total
  df_int_agg <- df_int_agg %>% 
    dplyr::mutate(lab = dplyr::case_when(fiscal_year == max(fiscal_year) ~ percent(exp_amt/exp_total, 1)))
  
  return(df_int_agg)
}
  
  

# VIZ ---------------------------------------------------------------------

viz_funding_distro <- function(df){
  
  ref_id <- "e258e5d3" #id for adorning to plots, making it easier to find on GH
  
  df %>% 
    ggplot2::ggplot(aes(fiscal_year, exp_amt, group = funding_type, fill = funding_type)) +
    ggplot2::geom_area(aes(y = exp_total), fill = "#909090", alpha = .2) +
    ggplot2::geom_area(alpha = .8) +
    ggplot2::geom_text(aes(label = lab), family = "Source Sans Pro", hjust = -.1,
              color = glitr::matterhorn, na.rm = TRUE) +
    ggplot2::facet_grid(~fct_reorder2(funding_type, fiscal_year, exp_amt)) +
    ggplot2::scale_y_continuous(label = number_format(scale = 1e-6, prefix = "$", suffix = "M"),
                       expand = c(.005, .005)) + 
    glitr::scale_fill_si("genoa", discrete = TRUE) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(x = NULL, y = NULL, fill = NA,
         subtitle = glue("{unique(df$funding_agency)}/{unique(df$country)}'s breakdown of annual expenditures by funding type"),
         caption = glue("Note: M&O and supply chain excluded
                        {metadata_fsd$caption}")) +
    glitr::si_style_ygrid() +
    ggplot2::theme(legend.position = "none")
  
}
  