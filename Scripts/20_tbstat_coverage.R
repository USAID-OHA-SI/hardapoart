# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  TB stat coverage by PSNUs
# REF ID:   70ebd4eb 
# LICENSE:  MIT
# DATE:     2023-02-21
# UPDATED: 


# MUNGE -------------------------------------------------------------------


prep_tbstat_cov <- function(df, cntry, agency, ...) {
  
  #clean exit if no data
  if(cntry %ni% unique(df$country) | agency %ni% unique(df$funding_agency))
    return(NULL)
  
  
  # filter dataset
  df_filtered <- df %>% 
    filter(indicator %in% c("TB_STAT", "TB_STAT_D"),
           country == cntry,
           funding_agency == agency,
           standardizeddisaggregate %in% c("Age/Sex/KnownNewPosNeg", "Age/Sex"))
  
  #verify filter worked
  df_filtered %>% 
    assertr::verify(indicator %in% c("TB_STAT", "TB_STAT_D") &
                      standardizeddisaggregate %in% c("Age/Sex/KnownNewPosNeg", "Age/Sex") &
                      funding_agency == agency &
                      country == cntry, 
                    error_fun = err_text(glue::glue("Error: {df} has not been filtered correctly. 
                                               Please check the first filter in prep_psnu_linkage().")), 
                    description = glue::glue("Verify that the filters worked")) 
  
  if(nrow(df_filtered) == 0)
    return(NULL)
  
  
  #bind in duplicative rows to serve as overall total for plot
  df_filtered <- df_filtered %>% 
    dplyr::bind_rows(df_filtered %>% 
                       dplyr::mutate(psnu = "OVERALL"))
  
  #group by, sum and reshape
  df_reshaped <- df_filtered %>% 
    dplyr::group_by(country, fiscal_year, funding_agency, indicator, psnu) %>% 
    dplyr::summarise(across(starts_with("qtr"), sum, na.rm = TRUE),
                     .groups = "drop") %>%
    gophr::reshape_msd() 
  
  #verify reshape
  df_reshaped %>% 
    assertr::verify("period" %in% names(df_reshaped), 
                    error_fun = err_text(glue::glue("Error: {df} has not been reshaped correctly and the period column does not exist. 
                                               Please check reshape_msd in prep_linkage_psnu().")), 
                    description = glue::glue("Verify that reshape_md worked"))
  
  
  #mutate coverage and add color
  df_viz <- df_reshaped %>% 
    pivot_wider(names_from = "indicator") %>% 
    mutate(coverage = `TB_STAT`/`TB_STAT_D`) %>% 
    mutate(fill_color = ifelse(coverage >= .95, genoa_light, burnt_sienna_light)) %>% 
    filter(period == max(period)) %>%
    dplyr::mutate(fill_color = case_when(psnu == "OVERALL" & coverage >= .95 ~ glitr::genoa,
                                         psnu == "OVERALL" & coverage < .95 ~ glitr::burnt_sienna,
                                         psnu!= "OVERALL" &  coverage >= .95 ~ glitr::genoa_light,
                                         psnu!= "OVERALL" &  coverage < .95 ~ glitr::burnt_sienna_light))
  
  return(df_viz)
  
}


  

# VIZ ---------------------------------------------------------------------------

# df_viz %>%
#  # dplyr::slice_max(order_by = TB_STAT_D, n = 21) %>% 
#  # filter(trendscoarse != "Unknown Age") %>% 
#   ggplot2::ggplot(aes(x = period)) +
#   ggplot2::geom_col(aes(y = `TB_STAT_D`,  color = color), fill = NA) +
#   ggplot2::geom_col(aes(y = `TB_STAT`, fill = color)) +
#   facet_wrap(~snu1, scales = "free_y") +
#   #ggplot2::facet_grid(trendscoarse ~ sex, scales = "free_y") +
#   si_style_xgrid() +
#   scale_fill_identity() +
#   scale_color_identity() +
#   coord_flip()


viz_tbstat_cov <- function(df) {

  q <- glue::glue("Is the percentage of TB patients that know their HIV status (TB_STAT % coverage) close to a 95% to align with the first 95 target?") %>% toupper
  
  if(is.null(df) || nrow(df) == 0)
    return(dummy_plot(q))
  
  ref_id <- "70ebd4eb"
  vrsn <- 1 
  
  cap_note <- ifelse(nrow(df) > 21, "Note: Limited to the largest 20 TB_STAT_D PSNUs\n", "")

  #limit to 21 bars (overall + 20 psnus)
  df <- df %>% 
    dplyr::slice_max(order_by = TB_STAT_D, n = 21)
  
  
  df %>%
    # dplyr::slice_max(order_by = `TB_STAT_D`, n = 21) %>% 
    ggplot(aes(coverage, forcats::fct_reorder(psnu, coverage), fill = fill_color)) +
    geom_col() +
    geom_vline(aes(xintercept = .95), linetype = "dashed", color = glitr::matterhorn) +
    geom_text(aes(label = percent(coverage, 1)), 
              size = 9/.pt, hjust = -.1,
              family = "Source Sans Pro", 
              color = glitr::matterhorn, 
              vjust = 0) +
    # geom_text(data = . %>% dplyr::slice_max(order_by = coverage, n = 1, with_ties = FALSE),
    #           aes(x = .95),
    #           label = glue::glue("95%"),
    #           family = "Source Sans Pro",
    #           color = glitr::matterhorn,
    #           vjust = -2) +
    expand_limits(x = c(0, 1.1)) + 
    coord_cartesian(clip = "off") +
    scale_x_continuous(expand = c(.005, .005)) +
    scale_fill_identity() +
    si_style_nolines() +
    labs(title = {q},
       subtitle = glue("{unique(df$funding_agency)}/{unique(df$country)} {metadata_msd$curr_pd} TB_STAT Coverage"),
         x = NULL, y = NULL,
         caption = glue("{cap_note} {metadata_msd$caption} | USAID/OHA/SIEI |  Ref id: {ref_id} v{vrsn}")) +
    theme(legend.position = "none",
          axis.text.x = element_blank())
  
}


  
