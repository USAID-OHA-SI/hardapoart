# PROJECT:  hardapoart
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  to develop a population pyramid viz
# REF ID:   aa8bd5b4 
# LICENSE:  MIT
# DATE:     2023-02-03
# UPDATED:  

# DEPENDENCIES -----------------------------------------------------------------
  # source("Scripts/91_setup.R")

# PREP -------------------------------------------------------------------------

  # df = df_natsubnat comes from 91_setup.R, could add a test to make sure 
  # it is actually natsubnat data
  # ou is a string, could add a unit test to make sure it's a valid OU

prep_pop_pyramid <- function(df, cntry){
  
  df_filt <- df %>%
    dplyr::filter(
      fiscal_year == max(fiscal_year),
      country == cntry,
      indicator == "PLHIV") %>%
    assertr::verify(indicator == "PLHIV" &
                    fiscal_year == max(fiscal_year) & 
                      country == cntry, 
                    error_fun = err_text(glue::glue("Error: {df} has not been filtered correctly. 
                                               Please check the first filter in prep_pop_pyramid().")), 
                    description = glue::glue("Verify that the filters worked")) %>%
    dplyr::select(fiscal_year, country, indicator, sex, ageasentered, targets) %>%
    dplyr::group_by(fiscal_year, country, indicator, sex, ageasentered) %>%
    dplyr::summarise(targets = sum(targets, na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::mutate(
      population = if_else(sex == "Male", targets*(-1), targets*1))
  
  unknown <- df_filt  %>%
    dplyr::filter(is.na(sex) & is.na(ageasentered)) %>%
    dplyr::mutate(n_unknown = comma(targets))
  
  df_viz <- df_filt %>%
    tidyr::drop_na(sex, ageasentered) %>%
    # include those with age and sex data unavailable in the notes on the viz
    # if there are no PLHIV missing age or sex data, replace NA with "No"
    # so that the note on the viz reads:
    # "Note: There are no PLHIV with unreported age and sex data.
    # otherwise it will show the number
    full_join(., unknown) %>%
    mutate(n_unknown = if_else(is.na(n_unknown) == TRUE, 
                               "no", n_unknown))
  
  return(df_viz)
  
  }

# VIZ --------------------------------------------------------------------------
  
  # df = df_natsubnat comes from 91_setup.R, could add a test to make sure 
  # it is actually natsubnat data
  # ou is a string, could add a unit test to make sure it's a valid OU
  # age is ageasenetered in MSD, categorized age (could be fct or char)
  # population is a numeric var for number of people in each age band
  # sex is a factor or char containing "Male" and "Female"
  # ref_id is an image reference id
  
  viz_pop_pyramid <- function(df){
    
    ref_id <- "aa8bd5b4"
    
    # pull in the number of PLHIV reported with no age or sex data available
    n_PLHIV_unknown <- df$n_unknown[1]
    
    # format the number with a comma if it is not "no"
    if(n_PLHIV_unknown != "no"){
      
      scales::comma(n_PLHIV_unknown)
      
      return(n_PLHIV_unknown)
    }
    
    df %>%
      ggplot2::ggplot(aes(x = ageasentered, y = population, fill = sex)) +
      ggplot2::geom_col() +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_manual(values = c("Male" = genoa, 
                                            "Female" = moody_blue)) +
      ggplot2::scale_y_continuous(
        # would be great to have it 
        # dynamically choose a scale 
        # based on the length of "value" since this can vary by OU 
        limits = c(min(df$population), max(df$population)),
        labels = function(x) {glue("{comma(abs(x))}")}, 
        # would like it to be able to use this ideally but
        # can't figure out how to use this and abs together
        # label_number(scale_cut = cut_short_scale())
      ) +
      ggplot2::labs(title = glue("Population Pyramid"),
                    x = NULL, y = NULL, fill = NULL,
                    subtitle = glue("{df$indicator[1]} | {metadata_natsubnat$curr_fy_lab}"),
                    caption = 
                      glue("Note: There are {n_PLHIV_unknown} PLHIV with unreported age and sex data.
                  Source: {metadata_natsubnat$curr_pd} MSD | Ref id: {ref_id} | US Agency for International Development")) +
      glitr::si_style_yline() +
      ggplot2::theme(
        panel.spacing = unit(.5, "line"),
        legend.position = "none",
        plot.title = element_markdown(),
        strip.text = element_markdown())
    
  }
