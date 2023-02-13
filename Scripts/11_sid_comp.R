# PROJECT:  hardapoart
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  gaps identified by SID
# REF ID:   79fe7ff5 
# LICENSE:  MIT
# DATE:     2023-02-02
# UPDATED: 
# NOTE:     based on catch-22/0220708_SID-scores-across-sutained-impact-cntries.R


# MUNGE -------------------------------------------------------------------

prep_sid <- function(df, cntry){
  
  if(cntry %ni% unique(df$operatingunit))
    return(NULL)
  
  #limit dataset
  df_int <- df %>% 
    dplyr::select(fiscal_year,  
                  country = operatingunit,
                  uuid = UUID,
                  sid_area = SIDQuestion,
                  sid_short_q = SIDshortquestion,
                  sid_score_raw = SIDraw,
                  sid_score_weighted = SIDweighted_answer) %>% 
    dplyr::filter(!is.na(sid_score_raw))
  
  #clean sid area
  df_int <- df_int %>% 
    dplyr::mutate(sid_area = stringr::str_remove_all(sid_area, "^[:digit:]{1,2}\\. |:$") %>% stringr::str_trim())
  
  #average score for each area by country
  df_int_weighted <- df_int %>% 
    dplyr::group_by(fiscal_year, country, sid_area) %>% 
    dplyr::summarise(avg_sid_score_weighted = sum(sid_score_weighted, na.rm = TRUE),
                     .groups = "drop")
  
  
  #create point for country, PEPFAR avg, and bounds for plotting
  df_viz <- df_int_weighted %>% 
    dplyr::mutate(val_cntry = dplyr::case_when(country == cntry ~ avg_sid_score_weighted)) %>% 
    dplyr::group_by(sid_area) %>% 
    dplyr::mutate(lower = min(avg_sid_score_weighted),
                  upper = max(avg_sid_score_weighted),
                  avg = mean(avg_sid_score_weighted),
                  lab = case_when(country == cntry ~ avg_sid_score_weighted)) %>% 
    dplyr::ungroup() 
  
  #color and group areas
  df_viz %>% 
    dplyr::group_by(sid_area) %>% 
    dplyr::mutate(fill_color = dplyr::case_when(country == cntry & avg_sid_score_weighted > avg ~ genoa,
                                                country == cntry ~ moody_blue,
                                                TRUE ~ trolley_grey_light),
                  font_color = ifelse(lab == max(lab, na.rm = TRUE), "white", matterhorn),
                  fct_grp = ifelse(val_cntry > avg, "Above PEPFAR Average", "Below PEPFAR Average")) %>%
    tidyr::fill(fct_grp, .direction = "downup") %>% 
    dplyr::ungroup() 
}

# VIZ ---------------------------------------------------------------------

viz_sid <- function(df){
  
  if(is.null(df))
    return(print(paste("No data available.")))
  
  ref_id <- "79fe7ff5" 
  vrsn <- 1 
  
  sel_cntry <- df %>% filter(!is.na(val_cntry)) %>% pull(country) %>% unique()
  
  df %>% 
    ggplot2::ggplot(aes(avg_sid_score_weighted, 
                        forcats::fct_reorder(sid_area, val_cntry, mean, .na_rm = TRUE))) +
    ggplot2::geom_linerange(aes(xmin = lower, xmax = upper), color = grey10k) +
    ggplot2::geom_point(color = "white", alpha = .2, na.rm = TRUE) +
    ggplot2::geom_point(alpha = .2, na.rm = TRUE) +
    ggplot2::geom_errorbar(aes(xmin = avg, xmax = avg), color = glitr::matterhorn) +
    ggplot2::geom_point(aes(x = val_cntry, fill = fill_color), 
                        color = glitr::matterhorn, shape = 21, size = 6, na.rm = TRUE) +
    ggplot2::geom_text(aes(x = val_cntry, label = number(lab, .1), color = font_color),
              na.rm = TRUE, family = "Source Sans Pro", size = 8/.pt) +
    ggplot2::expand_limits(x = c(0, 10)) +
    ggplot2::scale_x_continuous(expand = c(.005, .005)) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_color_identity() +
    ggplot2::labs(x = NULL, y = NULL,
                  title = glue("{sel_cntry} {unique(df$fiscal_year)} SID Scores") %>% toupper,
                  subtitle = glue("Large points represent {sel_cntry}'s score compared with other PEPFAR countries (smaller points) and PEPFAR average (line)"),
         caption = glue("{metadata_sid$caption} | USAID | Ref id: {ref_id} v{vrsn}")) +
    si_style_xgrid(facet_space = .5) +
    ggplot2::theme(axis.text.x = element_blank())
  

}
  
  