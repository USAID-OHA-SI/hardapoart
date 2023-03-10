# PROJECT:  hardapoart
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  hrh footprint
# REF ID:   d98b536f 
# LICENSE:  MIT
# DATE:     2023-02-06
# UPDATED:  2023-02-09

  
# MUNGE -------------------------------------------------------------------
  
prep_hrh_footprint <- function(df, cntry, agency){
 
  if(cntry %ni% unique(df$country) || agency %ni% unique(df$funding_agency))
    return(NULL)
  
  #limit to just the country/agency selected
  df <- df %>% 
    filter(country == cntry,
           funding_agency == agency)
  
  if(nrow(df) == 0)
    return(NULL)
  
  #assign funding type
  df_int <- gophr::apply_funding_type(df) 
  
  #total staff count
  df_total <- df_int %>% 
    dplyr::group_by(country, funding_agency, fiscal_year) %>% 
    dplyr::summarise(dplyr::across(c(individual_count, annual_fte, actual_annual_spend),
                                   \(x) sum(x, na.rm = TRUE)),
                     .groups = "drop") %>% 
    dplyr::mutate(cat = "",
                  subcat = "Total Staffing",
                  .before = 1)
  
  
  #staff count by SD vs NSD
  df_sd <- df_int %>% 
    dplyr::group_by(country, funding_agency, fiscal_year, funding_type) %>% 
    dplyr::summarise(dplyr::across(c(individual_count, annual_fte, actual_annual_spend),
                                   \(x) sum(x, na.rm = TRUE)),
                     .groups = "drop") %>% 
    dplyr::mutate(cat = "Service Delivery Type",
                  subcat = funding_type,
                  .before = 1) %>% 
    dplyr::select(-funding_type)
  
  #staff count by work location 
  df_loc <- df_int %>% 
    dplyr::group_by(country, funding_agency, fiscal_year, work_location) %>% 
    dplyr::summarise(dplyr::across(c(individual_count, annual_fte, actual_annual_spend),
                                   \(x) sum(x, na.rm = TRUE)),
                     .groups = "drop") %>% 
    dplyr::filter(work_location != "Validation Error") %>% 
    dplyr::mutate(cat = "Location",
                  subcat = work_location,
                  .before = 1) %>% 
    dplyr::select(-work_location)
  
  
  #staff count by ER category
  df_er <- df_int %>% 
    dplyr::group_by(country, funding_agency, fiscal_year, er_category) %>% 
    dplyr::summarise(dplyr::across(c(individual_count, annual_fte, actual_annual_spend),
                                   \(x) sum(x, na.rm = TRUE)),
                     .groups = "drop") %>% 
    dplyr::mutate(cat = "ER Category",
                  er_category = dplyr::recode(er_category,  "Program Management" = "PM."),
                  subcat = er_category,
                  .before = 1) %>% 
    dplyr::select(-er_category)
  
  
  #create labels
  df_viz <- df_total %>% 
    dplyr::bind_rows(df_sd, df_loc, df_er) %>%
    dplyr::group_by(cat) %>% 
    dplyr::mutate(share = annual_fte/sum(annual_fte, na.rm = TRUE)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(lab_val = dplyr::case_when(subcat == "Total Staffing" ~ glue::glue("**{subcat}**<br>FTEs: {scales::number_format(accuracy = .1, scale_cut = cut_short_scale())(individual_count)}<br>Individual Staff: {scales::number_format(accuracy = .1, scale_cut = cut_short_scale())(annual_fte)} | Actual Annual Spend: {scales::number_format(accuracy = .1, prefix = '$', scale_cut = cut_short_scale())(actual_annual_spend)}"),
                                   share >=.12 ~ glue::glue("**{subcat}**<br>{scales::number_format(accuracy = .1, scale_cut = cut_short_scale())(individual_count)}<br>{scales::number_format(accuracy = .1, scale_cut = cut_short_scale())(annual_fte)} | {scales::number_format(accuracy = .1, prefix = '$', scale_cut = cut_short_scale())(actual_annual_spend)}")),
                  lab_90 = dplyr::case_when(is.na(lab_val) ~ subcat))
  
  #order categories (and number for color mapping)
  df_viz <- df_viz %>% 
    dplyr::mutate(cat = fct_inorder(cat) %>% fct_rev) %>% 
    dplyr::arrange(cat, annual_fte) %>% 
    dplyr::mutate(subcat = forcats::fct_inorder(subcat)) %>% 
    dplyr::group_by(cat) %>% 
    dplyr::mutate(row = row_number())
  
  #create a color map
  df_colors <- tibble::tibble(cat = "",
                              row = 1) %>% 
    dplyr::bind_rows(
      tidyr::expand_grid(cat = c("ER Category", "Location", "Service Delivery Type"),
                         row = 1:4)) %>% 
    dplyr::bind_cols(fill_color = c(glitr::old_rose,
                                    glitr::si_rampr("burnt_siennas", 4, alpha = .6),
                                    glitr::si_rampr("moody_blues", 4, alpha = .6),
                                    glitr::si_rampr("scooters", 4, alpha = .6))) %>% 
    dplyr::mutate(cat = forcats::fct_inorder(cat),
                  text_color = dplyr::case_when(cat == "" ~ "white", 
                                                row == 4 ~ glitr::si_palettes$trolley_greys[1], 
                                                TRUE~ glitr::matterhorn))
  
  #join colors to data
  df_viz <- dplyr::left_join(df_viz, df_colors, by = join_by(cat, row))
  
  return(df_viz)
}

  

# VIZ ---------------------------------------------------------------------

viz_hrh_footprint <- function(df){
 
  q <- glue::glue("What does the current staffing footprint look like? How do they align with sustainability and leveraging our assets?") %>% toupper 
    
   if(is.null(df) || nrow(df) == 0)
    return(dummy_plot(q))
  
  q <- stringr::str_replace(q, "LIKE", glue::glue("LIKE IN {toupper(unique(df$country))}")) %>% stringr::str_wrap(width = 80)
  
  ref_id <- "d98b536f" #id for adorning to plots, making it easier to find on GH
  vrsn <- 1
  
  df %>% 
    ggplot2::ggplot(aes(annual_fte, cat, fill = fill_color, group = subcat)) +
    ggplot2::geom_col(color = "white") +
    ggtext::geom_richtext(aes(label = lab_val, color = text_color), position = position_stack(vjust = .5),
                           label.color = NA, fill = NA, na.rm = TRUE,
                           family = "Source Sans Pro") +
    ggtext::geom_richtext(aes(label = lab_90, color = text_color), angle = 90, position = position_stack(vjust = .5),
                          label.color = NA, fill = NA, na.rm = TRUE, size = 8/.pt,
                          family = "Source Sans Pro") +
    ggplot2::facet_wrap(.~fct_rev(cat), scales = "free", ncol = 1) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_color_identity() +
    ggplot2::labs(x = NULL, y = NULL,
                  title = {q},
                  subtitle = glue("{metadata_hrh$curr_fy_lab} HRH Staffing Footprint in {unique(df$funding_agency)}/{unique(df$country)} Broken Down By FTEs"),
                  caption = glue("{metadata_hrh$caption} Structured Dataset (not redacted) | USAID/OHA/SIEI |  Ref id: {ref_id} v{vrsn}")) + 
    ggplot2::coord_cartesian(clip = "off") +
    glitr::si_style_nolines() +
    ggplot2::theme(legend.position = "none",
                   axis.text = element_blank(),
                   panel.spacing = unit(.1, "picas")) 
  
}  
