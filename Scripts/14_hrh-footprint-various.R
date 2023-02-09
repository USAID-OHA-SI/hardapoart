# PROJECT:  hardapoart
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  hrh footprint
# REF ID:   d98b536f 
# LICENSE:  MIT
# DATE:     2023-02-06
# UPDATED: 

  
# MUNGE -------------------------------------------------------------------
  
prep_hrh_footprint <- function(df, cntry){
 
  #assign funding type
  df_int <- apply_funding_type(df) 
  
  #total staff count
  df_total <- df_int %>% 
    group_by(fiscal_year) %>% 
    summarise(across(c(individual_count, annual_fte, actual_annual_spend),
                     \(x) sum(x, na.rm = TRUE)),
              .groups = "drop") %>% 
    mutate(cat = "",
           subcat = "Total Staffing",
           .before = 1)
  
  
  #staff count by SD vs NSD
  df_sd <- df_int %>% 
    group_by(fiscal_year, funding_type) %>% 
    summarise(across(c(individual_count, annual_fte, actual_annual_spend),
                     \(x) sum(x, na.rm = TRUE)),
              .groups = "drop") %>% 
    mutate(cat = "Service Delivery Type",
           subcat = funding_type,
           .before = 1) %>% 
    select(-funding_type)
  
  #staff count by work location 
  df_loc <- df_int %>% 
    group_by(fiscal_year, work_location) %>% 
    summarise(across(c(individual_count, annual_fte, actual_annual_spend),
                     \(x) sum(x, na.rm = TRUE)),
              .groups = "drop") %>% 
    filter(work_location != "Validation Error") %>% 
    mutate(cat = "Location",
           subcat = work_location,
           .before = 1) %>% 
    select(-work_location)
  
  
  #staff count by ER category
  df_er <- df_int %>% 
    group_by(fiscal_year, er_category) %>% 
    summarise(across(c(individual_count, annual_fte, actual_annual_spend),
                     \(x) sum(x, na.rm = TRUE)),
              .groups = "drop") %>% 
    mutate(cat = "ER Category",
           er_category = recode(er_category,  "Program Management" = "PM"),
           subcat = er_category,
           .before = 1) %>% 
    select(-er_category)
  
  
  #create labels
  df_viz <- df_total %>% 
    bind_rows(df_sd, df_loc, df_er) %>%
    mutate(lab_val = ifelse(subcat == "Total Staffing",
                            glue("**{subcat}**<br>FTEs: {number_format(accuracy = .1, scale_cut = cut_short_scale())(individual_count)}<br>Individual Staff: {number_format(accuracy = .1, scale_cut = cut_short_scale())(annual_fte)} | Actual Annual Spend: {number_format(accuracy = .1, prefix = '$', scale_cut = cut_short_scale())(actual_annual_spend)}"),
                            glue("**{subcat}**<br>{number_format(accuracy = .1, scale_cut = cut_short_scale())(individual_count)}<br>{number_format(accuracy = .1, scale_cut = cut_short_scale())(annual_fte)} | {number_format(accuracy = .1, prefix = '$', scale_cut = cut_short_scale())(actual_annual_spend)}")))
  
  #order categories (and number for color mapping)
  df_viz <- df_viz %>% 
    mutate(cat = fct_inorder(cat) %>% fct_rev) %>% 
    arrange(cat, individual_count) %>% 
    mutate(subcat = fct_inorder(subcat)) %>% 
    group_by(cat) %>% 
    mutate(row = row_number())
  
  #create a color map
  df_colors <- tibble::tibble(cat = "",
                      row = 1) %>% 
    dplyr::bind_rows(
      tidyr::expand_grid(cat = c("ER Category", "Location", "Service Delivery Type"),
                  row = 1:4)) %>% 
    dplyr::bind_cols(fill_color = c(glir::old_rose,
                             glitr::si_rampr("burnt_siennas", 4, alpha = .6),
                             glitr::si_rampr("moody_blues", 4, alpha = .6),
                             glitr::si_rampr("scooters", 4, alpha = .6))) %>% 
    dplyr::mutate(cat = forcats::fct_inorder(cat),
           text_color = dplyr::case_when(cat == "" ~ "white", 
                                  row == 4 ~ glitr::si_palettes$trolley_greys[1], 
                                  TRUE~ glitr::matterhorn))
  
  #join colors to data
  df_viz <- dplyr::left_join(df_viz, df_colors, by = join_by(cat, row))
}

  

# VIZ ---------------------------------------------------------------------

viz_hrh_footprint <- function(df){
  
  ref_id <- "d98b536f" #id for adorning to plots, making it easier to find on GH
  
  df %>% 
    ggplot2::ggplot(aes(individual_count, cat, fill = fill_color, group = subcat)) +
    ggplot2::geom_col(color = "white") +
    ggplot2::geom_richtext(aes(label = lab_val, color = text_color), position = position_stack(vjust = .5),
                           label.color = NA, fill = NA,
                           family = "Source Sans Pro") +
    ggplot2::facet_wrap(.~fct_rev(cat), scales = "free", ncol = 1) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_color_identity() +
    ggplot2::labs(x = NULL, y = NULL,
                  subtitle = glue("{metadata_hrh$curr_fy_lab} HRH Staffing Footprint in {unique(df$cntry)} Broken Down By FTEs"),
                  caption = glue("metadata_hrh$caption Structured Dataset (not redacted) | Ref id: {ref_id}")) + 
    glitr::si_style_nolines() +
    ggplot2::theme(legend.position = "none",
                   axis.text = element_blank(),
                   panel.spacing = unit(.1, "picas")) 
  
}  
  
  
  
# #top staff by title
# df_viz2 <- df_hrh_lim %>% 
#   group_by(fiscal_year, employment_title) %>% 
#   summarise(across(c(individual_count, annual_fte, actual_annual_spend),
#                    \(x) sum(x, na.rm = TRUE)),
#             .groups = "drop") %>% 
#   slice_max(order_by = annual_fte, n = 10) %>% 
#   mutate(lab_val = ifelse(annual_fte == max(annual_fte),
#                           glue("**{employment_title}**<br>FTEs: {number_format(accuracy = .1, scale_cut = cut_short_scale())(individual_count)} | Staff: {number_format(accuracy = .1, scale_cut = cut_short_scale())(annual_fte)} | Annual Spend: {number_format(accuracy = .1, prefix = '$', scale_cut = cut_short_scale())(actual_annual_spend)}"),
#                           glue("**{employment_title}**<br>{number_format(accuracy = .1, scale_cut = cut_short_scale())(individual_count)} | {number_format(accuracy = .1, scale_cut = cut_short_scale())(annual_fte)} | {number_format(accuracy = .1, prefix = '$', scale_cut = cut_short_scale())(actual_annual_spend)}")))  
# 
# v2 <- df_viz2 %>% 
#   ggplot(aes(annual_fte, fct_reorder(lab_val, annual_fte))) +
#   geom_col(fill = genoa, alpha = .7) +
#   scale_x_continuous(expand = c(.005, .005), label = comma) +
#   labs(x = NULL, y = NULL,
#        subtitle = glue("Top FTEs Position Titles in {cntry}"),
#        caption = glue("Source: FY21-22c SitexIM HRH Structured Dataset (not redacted) | Ref id: {ref_id}")) + 
#   si_style_xgrid() +
#   theme(axis.text.y = element_markdown())
# 
# 
# 
# v1 + v2 +
#   plot_layout(widths = c(2, 1))