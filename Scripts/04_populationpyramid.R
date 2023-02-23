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

  prep_pop_pyramid <- function(df, cntry, ...){

  # clean exit if no data
  if(cntry %ni% unique(df$country))
    return(NULL)
  
  df_filt <- df %>%
    dplyr::filter(
      fiscal_year == max(fiscal_year),
      country == cntry)
  
  if(nrow(df_filt) == 0)
    return(NULL)
  
  df_filt %>%
    assertr::verify(fiscal_year == max(fiscal_year) &
                    country == cntry,
                    error_fun = err_text(glue::glue("Error: {df} has not been filtered correctly.
                                               Please check the first filter in prep_pop_pyramid().")),
                    description = glue::glue("Verify that the filters worked"))
  
  df_filt <- df_filt %>%
    dplyr::select(fiscal_year, country, indicator, sex, ageasentered, targets) %>%
    dplyr::group_by(fiscal_year, country, indicator, sex, ageasentered) %>%
    dplyr::summarise(targets = sum(targets, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(population = if_else(sex == "Male", -targets, targets)) %>%
    dplyr::filter(indicator %in% c("POP_EST", "PLHIV"))
  
  plhiv_unknown <- df_filt  %>%
    dplyr::filter(
      indicator == "PLHIV", 
      is.na(sex) & is.na(ageasentered)) %>%
    dplyr::mutate(n_unknown = comma(targets))
  
  popest_unknown <- df_filt  %>%
    dplyr::filter(
      indicator == "POP_EST", 
      is.na(sex) & is.na(ageasentered)) %>%
    dplyr::mutate(n_unknown = comma(targets))
  
  df_viz <- df_filt %>%
    tidyr::drop_na(sex, ageasentered) %>%
    # include those with age and sex data unavailable in the notes on the viz
    # if there are no PLHIV/POP_EST missing age or sex data, replace NA with "No"
    # so that the note on the viz reads:
    # "Note: There are no PLHIV/POP_EST with unreported age and sex data.
    # otherwise it will show the number
    full_join(., plhiv_unknown,
              by = join_by(fiscal_year, country, indicator, sex, ageasentered, targets,
                           population)) %>%
    full_join(., popest_unknown,
              by = join_by(fiscal_year, country, indicator, sex, ageasentered, targets,
                           population, n_unknown)) %>%
    mutate(plhiv_unknown_sentence = case_when(
                                  (indicator == "PLHIV" & is.na(n_unknown) == TRUE)
                                   ~glue::glue("There are no unreported age and sex estimates for the Populaton Living with HIV indicator."), 
                                  
                                  (indicator == "PLHIV" & is.na(n_unknown) == FALSE)
                                   ~glue::glue("There are {n_unknown} unreported age and sex estimates for the Populaton Living with HIV indicator.")),
           
           popest_unknown_sentence = case_when(
                                  (indicator == "POP_EST" & is.na(n_unknown) == TRUE)
                                   ~ glue::glue("There are no unreported age and sex estimates for the Populaton (Estimated) indicator."), 
                                  (indicator == "POP_EST" & is.na(n_unknown) == FALSE)
                                   ~ glue::glue("There are {n_unknown} unreported age and sex estimates for the Populaton (Estimated) indicator.")),
           plhiv_unknown_sentence = replace_na(plhiv_unknown_sentence, popest_unknown_sentence[1]), 
           popest_unknown_sentence = replace_na(popest_unknown_sentence, plhiv_unknown_sentence[1]), 
           unknown_sentences = glue::glue("{plhiv_unknown_sentence} {popest_unknown_sentence}"),
           indicator_label = if_else(indicator == "POP_EST", "Population (Estimated)", 
                                     "Population Living with HIV"))
  return(df_viz)
  
}

# VIZ --------------------------------------------------------------------------
  
  # df = df_natsubnat comes from 91_setup.R, could add a test to make sure 

  viz_pop_pyramid <- function(df){
  
  if(is.null(df) || nrow(df) == 0)
    return(print(paste("No data available.")))
  
  ref_id <- "aa8bd5b4"
  vrsn <- 1.2
  
  df %>%
    ggplot2::ggplot(aes(population, ageasentered, fill = sex)) +
    ggplot2::geom_col() +
    ggplot2::geom_vline(aes(xintercept = 0), color = "white", linewidth = 1.1)+
    ggplot2::scale_fill_manual(values = c("Male" = glitr::genoa, 
                                          "Female" = glitr::moody_blue)) +
    facet_wrap(~indicator_label, scales = "free_x") +
    ggplot2::scale_x_continuous(
      # while it's nice to have a consistent access, and this may work for 
      # OUs with larger programs, it doesn't work for OUs with smaller programs
     #limits = c(-max(df$targets), max(df$targets)),
      labels = function(x) {glue("{label_number(scale_cut = cut_short_scale())(abs(x))}")}) +
    ggplot2::labs(title = glue("{unique(df$country)} - {unique(df$fiscal_year)} {df$indicator_label} Pyramid") %>% toupper,
                  subtitle =  glue::glue("Comparison between <span style='color:{genoa}'>Males</span> & <span style='color:{moody_blue}'>FEMALES</span> by age band"),
                  x = NULL, y = NULL, fill = NULL,
                  caption = 
                    glue("{df$unknown_sentences[1]}
                         {metadata_natsubnat$caption} | USAID/OHA/SIEI |  Ref id: {ref_id} v{vrsn}")) +
    glitr::si_style_yline() +
    ggplot2::theme(
      legend.position = "none",
      plot.subtitle = element_markdown())
  
  }
  
# # Example 
# 
# # small program OU
# df_ssd <- prep_pop_pyramid(df_natsubnat, "South Sudan")
# viz_pop_pyramid(df_ssd)
# 
# # larger program OU
# df_mwi <- prep_pop_pyramid(df_natsubnat, "Malawi")
# viz_pop_pyramid(df_mwi)