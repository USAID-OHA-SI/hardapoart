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

  prep_pop_pyramid <- function(df, cntry, selected_ind, ...){
  
  # clean exit if no data
  if(cntry %ni% unique(df$country))
    return(NULL)
  
  # clean exit if missing required indicators
  assertr::verify(df, 
                  selected_ind %in% indicator,
                  error_fun = err_text(glue::glue("Error: {df} does not contain the required indicators PLHIV or POP_EST.
                                               Please check {df} for at least one of these indicators.")),
                  description = glue::glue("Verify dataset has required indicators"))
  
  if(selected_ind %ni% df$indicator)
    return(NULL)
  
  df_filt <- df %>%
    dplyr::filter(
      fiscal_year == max(fiscal_year),
      country == cntry, 
      # can be either POP_EST or PLHIV 
      # need to figure out a way to limit user choice on this
      # idea for 1.2:
      # add option for both as a faceted plot
      indicator == selected_ind)
  
  if(nrow(df_filt) == 0)
    return(NULL)
  
  df_filt %>%
    assertr::verify(indicator == selected_ind &
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
    # if there are no PLHIV/POP_EST missing age or sex data, replace NA with "No"
    # so that the note on the viz reads:
    # "Note: There are no PLHIV/POP_EST with unreported age and sex data.
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

  viz_pop_pyramid <- function(df){
  
  if(is.null(df) || nrow(df) == 0)
    return(print(paste("No data available.")))
  
  ref_id <- "aa8bd5b4"
  vrsn <- 1.1
  indicator <- unique(df$indicator)[1]
  
  # pull in the number of PLHIV/POP_EST reported with no age or sex data available
  n_PPL_unknown <- df$n_unknown[1]
  
  # format the number with a comma if it is not "no"
  if(n_PPL_unknown != "no"){
    
    scales::comma(n_PPL_unknown)
    
    return(n_PPL_unknown)
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
    ggplot2::labs(title = glue("{unique(df$country)} - {unique(df$fiscal_year)} {indicator} Pyramid") %>% toupper,
                  subtitle =  glue::glue("Comparison between <span style='color:{genoa}'>Males</span> & <span style='color:{moody_blue}'>FEMALES</span> {indicator} by age band"),
                  x = NULL, y = NULL, fill = NULL,
                  caption = 
                    glue("Note: There are {n_PPL_unknown} {indicator} with unreported age and sex data.
                  {metadata_natsubnat$caption} | USAID/OHA/SIEI |  Ref id: {ref_id} v{vrsn}")) +
    glitr::si_style_yline() +
    ggplot2::theme(
      legend.position = "none",
      plot.subtitle = element_markdown())
  
  }
  
# Example 
  
# df_zmb <- prep_pop_pyramid(df_natsubnat, "Zambia", "PLHIV")
# 
# viz_pop_pyramid(df_zmb)
  
# df_zmb <- prep_pop_pyramid(df_natsubnat, "Zambia", "POP_EST")
# 
# viz_pop_pyramid(df_zmb)

