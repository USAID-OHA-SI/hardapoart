# PROJECT:  hardapoart
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  population pyramid viz
# REF ID:   aa8bd5b4 
# LICENSE:  MIT
# DATE:     2023-02-03
# UPDATED:  2023-02-17

# PREP -------------------------------------------------------------------------

  # df = df_natsubnat comes from 91_setup.R, could add a test to make sure 
  # it is actually natsubnat data
  # cntry is a string
  # selected_ind is either "POP_EST" or "PLHIV"

  prep_pop_pyramid <- function(df, cntry){
  
  # clean exit if no data
  if(cntry %ni% unique(df$country))
    return(NULL)
  
  df_filt <- df %>%
    dplyr::filter(
      fiscal_year == max(fiscal_year),
      country == cntry, 
      indicator %in% c("PLHIV", "POP_EST"))
  
  if(nrow(df_filt) == 0)
    return(NULL)
  
  df_filt %>%
    assertr::verify(indicator %in% c("PLHIV", "POP_EST") &
                      fiscal_year == max(fiscal_year) &
                      country == cntry,
                    error_fun = err_text(glue::glue("Error: {df} has not been filtered correctly.
                                               Please check the first filter in prep_pop_pyramid().")),
                    description = glue::glue("Verify that the filters worked"))
  
  df_filt <- df_filt %>%
    dplyr::mutate(indicator = ifelse(indicator == "POP_EST", "Population (Est)", indicator)) %>% 
    dplyr::group_by(fiscal_year, country, indicator, sex, ageasentered) %>%
    dplyr::summarise(targets = sum(targets, na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::mutate(population = if_else(sex == "Male", -targets, targets))
  
  df_viz <- df_filt %>%
    tidyr::drop_na(sex, ageasentered)

  df_viz <- df_viz %>% 
    dplyr::group_by(indicator) %>% 
    dplyr::mutate(axis_max = max(targets, na.rm = TRUE),
                  axis_min = -axis_max) %>% 
    dplyr::ungroup()
    
  return(df_viz)
  
}

# VIZ --------------------------------------------------------------------------
  
  # df = df_natsubnat comes from 91_setup.R, could add a test to make sure 

  viz_pop_pyramid <- function(df){
  
    q <- glue::glue("Is there a youth bulge {unique(df$country)} needs to plan for?") %>% toupper
      
  if(is.null(df) || nrow(df) == 0)
    return(dummy_plot(q))
  
  ref_id <- "aa8bd5b4"
  vrsn <- 2
  
  
  df %>%
    ggplot2::ggplot(aes(population, ageasentered, fill = sex)) +
    ggplot2::geom_blank(aes(axis_max)) +
    ggplot2::geom_blank(aes(axis_min)) +
    ggplot2::geom_col(alpha = .8, na.rm = TRUE) +
    ggplot2::geom_vline(aes(xintercept = 0), color = "white", linewidth = 1.1)+
    ggplot2::facet_wrap(~forcats::fct_rev(indicator), scales = "free_x") +
    ggplot2::scale_fill_manual(values = c("Male" = glitr::genoa, 
                                          "Female" = glitr::moody_blue)) +
    ggplot2::scale_x_continuous(
      labels = function(x) {glue("{label_number(scale_cut = cut_short_scale())(abs(x))}")}, 
    ) +
    ggplot2::labs(title = {q},
                  subtitle =  glue::glue("Comparison between <span style='color:{genoa}'>Males</span> & <span style='color:{moody_blue}'>Females</span> by age band"),
                  x = NULL, y = NULL, fill = NULL,
                  caption = 
                    glue("{metadata_natsubnat$caption} | USAID/OHA/SIEI |  Ref id: {ref_id} v{vrsn}")) +
    glitr::si_style_yline() +
    ggplot2::theme(
      legend.position = "none",
      strip.text = element_text(hjust = .5),
      plot.subtitle = element_markdown())
  
  }
  
# Example 
  
# df_zmb <- prep_pop_pyramid(df_natsubnat, "Zambia", "PLHIV")
# 
# viz_pop_pyramid(df_zmb)
  
# df_zmb <- prep_pop_pyramid(df_natsubnat, "Zambia", "POP_EST")
# 
# viz_pop_pyramid(df_zmb)

