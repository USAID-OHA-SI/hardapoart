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
  
  #clean exit if no data
  if(cntry %ni% unique(df$country))
    return(NULL)
  
  df_filt <- df %>%
    dplyr::filter(
      fiscal_year == max(fiscal_year),
      country == cntry,
      indicator == "PLHIV")
  
  if(nrow(df_filt) == 0)
    return(NULL)
  
  df_filt %>%
    assertr::verify(indicator == "PLHIV" &
                    fiscal_year == max(fiscal_year) & 
                      country == cntry, 
                    error_fun = err_text(glue::glue("Error: {df} has not been filtered correctly. 
                                               Please check the first filter in prep_pop_pyramid().")), 
                    description = glue::glue("Verify that the filters worked")) 
  
  df_filt <- df_filt %>%
    dplyr::select(fiscal_year, country, indicator, sex, ageasentered, targets) %>%
    dplyr::group_by(fiscal_year, country, indicator, sex, ageasentered) %>%
    dplyr::summarise(targets = sum(targets, na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::mutate(population = if_else(sex == "Male", -targets, targets))
  
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
    full_join(., unknown, 
              by = join_by(fiscal_year, country, indicator, sex, ageasentered, targets,
                           population)) %>%
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
    
    if(is.null(df) || nrow(df) == 0)
      return(print(paste("No data available.")))
    
    ref_id <- "aa8bd5b4"
    vrsn <- 1 
    
    # pull in the number of PLHIV reported with no age or sex data available
    n_PLHIV_unknown <- df$n_unknown[1]
    
    # format the number with a comma if it is not "no"
    if(n_PLHIV_unknown != "no"){
      
      scales::comma(n_PLHIV_unknown)
      
      return(n_PLHIV_unknown)
    }
    
    df %>%
      ggplot2::ggplot(aes(population, ageasentered, fill = sex)) +
      ggplot2::geom_col() +
      ggplot2::geom_vline(aes(xintercept = 0), color = "white", linewidth = 1.1)+
      ggplot2::scale_fill_manual(values = c("Male" = glitr::genoa, 
                                            "Female" = glitr::moody_blue)) +
      ggplot2::scale_x_continuous(
        limits = c(-max(df$targets), max(df$targets)),
        labels = function(x) {glue("{label_number(scale_cut = cut_short_scale())(abs(x))}")}, 
      ) +
      ggplot2::labs(title = glue("{unique(df$country)} - {unique(df$fiscal_year)} PLHIV Pyramid") %>% toupper,
                    subtitle =  glue::glue("Comparison between <span style='color:{genoa}'>Male</span> & <span style='color:{moody_blue}'>Female</span> PLHIV by age band"),
                    x = NULL, y = NULL, fill = NULL,
                    caption = 
                      glue("Note: There are {n_PLHIV_unknown} PLHIV with unreported age and sex data.
                  {metadata_natsubnat$caption} | USAID/OHA/SIEI |  Ref id: {ref_id} v{vrsn}")) +
      glitr::si_style_yline() +
      ggplot2::theme(
        legend.position = "none",
        plot.subtitle = element_markdown())
    
  }
