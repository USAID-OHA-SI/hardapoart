# PROJECT:  hardapoart
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  to develop a population pyramid viz
# REF ID:   aa8bd5b4 
# LICENSE:  MIT
# DATE:     2023-02-03
# UPDATED:  

# DEPENDENCIES -----------------------------------------------------------------
  source("Scripts/91_setup.R")

# GLOBAL VARIABLES -------------------------------------------------------------
  ref_id <- "aa8bd5b4"
  
# MUNGE ------------------------------------------------------------------------
  
  # df_natsubnat comes from 91_setup.R, could add a test to make sure it is actually natsubnat data
  # ou is a string, could add a unit test to make sure it's a valid OU
  
  prep_pop_pyramid <- function(.df, .ou,...){
  
    df_filt <- .df %>%
    dplyr::filter(
      fiscal_year == metadata$curr_fy,
      operatingunit == .ou,
      indicator == "PLHIV") %>%
    dplyr::select(fiscal_year, operatingunit, indicator, sex, ageasentered, targets) %>%
    dplyr::group_by(fiscal_year, operatingunit, indicator, sex, ageasentered) %>%
    dplyr::summarise(across(targets, \(x) sum(x, na.rm = TRUE))) %>%
    dplyr::mutate(
      population = if_else(sex == "Male", targets*(-1), targets*1))
  
  unknown <- df_filt  %>%
    dplyr::filter(is.na(sex) & is.na(ageasentered)) %>%
    dplyr::mutate(n_unknown = comma(targets))
  
  df_viz <- df_filt %>%
    tidyr::drop_na(sex, ageasentered)
  
  return(df_viz)
  
  }
  
  ou <- "South Sudan"
  
  df_viz <- prep_pop_pyramid(df_natsubnat, ou)
  
# VIZ --------------------------------------------------------------------------
  
  viz_pop_pyramid <- function(.df, .age_var, .pop_var, .sex_var, ...){
    
    .df %>%
      ggplot2::ggplot(aes(x = .age_var, y = .pop_var, fill = .sex_var)) +
      ggplot2::geom_col() +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_manual(values = c("Male" = genoa, 
                                            "Female" = moody_blue)) +
      ggplot2::scale_y_continuous(
        # would be great to have it 
        # dynamically choose a scale 
        # based on the length of "value" since this can vary by OU 
        limits = c(min(df_viz$population), max(df_viz$population)),
        labels = function(x) {glue("{comma(abs(x))}")}, 
        # would like it to be able to use this ideally but
        # can't figure out how to use this and abs together
        # label_number(scale_cut = cut_short_scale())
      ) +
      ggplot2::labs(title = glue("Population Pyramid"),
                    x = NULL, y = NULL, fill = NULL,
                    subtitle = glue("{.df$indicator[1]} | {metadata$curr_fy_lab}"),
                    caption = 
                      glue("Note: There are {unknown$n_unknown[1]} PLHIV with unreported age and sex data.
                  Source: {metadata$curr_pd} MSD | Ref id: {ref_id} | US Agency for International Development")) +
      glitr::si_style_yline() +
      ggplot2::theme(
        panel.spacing = unit(.5, "line"),
        legend.position = "none",
        plot.title = element_markdown(),
        strip.text = element_markdown())
    
  }
  
  # i think there is a better way to do this in ggplot2 but this works!
  viz_pop_pyramid(df_viz, df_viz$ageasentered, df_viz$population, df_viz$sex)
  
  glitr::si_save("Images/8_pop_pyramid.png")