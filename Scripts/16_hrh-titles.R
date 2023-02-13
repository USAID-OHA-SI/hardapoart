# PROJECT:  hardapoart
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  hrh top titles
# REF ID:   9831a46a 
# LICENSE:  MIT
# DATE:     2023-02-09
# UPDATED: 

# MUNGE -------------------------------------------------------------------

  prep_hrh_titles <- function(df, cntry, agency){
    

    if(cntry %ni% unique(df$country) | agency %ni% unique(df$funding_agency))
      return(NULL)
    
    #limit to just the country/agency selected
    df <- df %>% 
      filter(country == cntry,
             funding_agency == agency)
    
    #top staff by title
    df <- df %>%
      dplyr::group_by(country, funding_agency, fiscal_year, employment_title) %>%
      dplyr::summarise(dplyr::across(c(individual_count, annual_fte, actual_annual_spend),
                                     \(x) sum(x, na.rm = TRUE)),
                       .groups = "drop") %>%
      dplyr::slice_max(order_by = annual_fte, n = 10) %>%
      dplyr::mutate(lab_val = ifelse(annual_fte == max(annual_fte),
                                     glue::glue("**{employment_title}**<br>FTEs: {scales::number_format(accuracy = .1, scale_cut = cut_short_scale())(individual_count)} | Staff: {scales::number_format(accuracy = .1, scale_cut = cut_short_scale())(annual_fte)} | Annual Spend: {scales::number_format(accuracy = .1, prefix = '$', scale_cut = cut_short_scale())(actual_annual_spend)}"),
                                     glue::glue("**{employment_title}**<br>{scales::number_format(accuracy = .1, scale_cut = cut_short_scale())(individual_count)} | {scales::number_format(accuracy = .1, scale_cut = cut_short_scale())(annual_fte)} | {scales::number_format(accuracy = .1, prefix = '$', scale_cut = cut_short_scale())(actual_annual_spend)}")))
  
    return(df)
    
  }
 

# VIZ ---------------------------------------------------------------------

 viz_hrh_titles <- function(df){

   if(is.null(df))
     return(print(paste("No data available.")))

   ref_id <- "9831a46a" #id for adorning to plots, making it easier to find on GH
   vrsn <- 1
   
   df %>%
     ggplot2::ggplot(aes(annual_fte, forcats::fct_reorder(lab_val, annual_fte))) +
     ggplot2::geom_col(fill = glitr::genoa, alpha = .7) +
     ggplot2::scale_x_continuous(expand = c(.005, .005), label = comma) +
     ggplot2::labs(x = NULL, y = NULL,
                   subtitle = glue::glue("Top FTEs Position Titles in {unique(df$funding_agency)}/{unique(df$country)}"),
                   caption = glue::glue("{metadata_hrh$caption} Structured Dataset (not redacted) | USAID | Ref id: {ref_id} v{vrsn}")) +
     glitr::si_style_xgrid() +
     ggplot2::theme(axis.text.y = element_markdown())
   
 }  

