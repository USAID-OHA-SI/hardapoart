# PROJECT:  hardapoart
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  hrh footprint
# REF ID:   d98b536f 
# LICENSE:  MIT
# DATE:     2023-02-06
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(gagglr)
  library(glue)
  library(scales)
  library(extrafont)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(readxl)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "d98b536f" #id for adorning to plots, making it easier to find on GH
  
  # get_metadata(type = "HRH") #list of MSD metadata elements

  cntry <- "Malawi"
  
  temp_folder(TRUE)
  
# IMPORT ------------------------------------------------------------------
  
  si_path() %>% 
    return_latest("HRH") %>%
    unzip(exdir = folderpath_tmp)
  
  df_hrh <- folderpath_tmp %>% 
    return_latest("HRH.*xlsx") %>%
    read_excel(col_types = "text")
  
# MUNGE -------------------------------------------------------------------
  
  #filter to select country
  df_hrh_lim <- df_hrh %>% 
    filter(fiscal_year == max(fiscal_year),
           country == cntry)
  
  #fy for viz
  fy <- glue("FY{max(df_hrh_lim$fiscal_year) %>% str_sub(-2)}")

  #convert col types
  df_hrh_lim <- df_hrh_lim %>% 
    mutate(across(c(fiscal_year, individual_count), \(x) as.integer(x)),
           across(c(annual_expenditure, annual_fringe, actual_annual_spend, 
                    equivalent_annual_spend, equivalent_annual_salary, avg_fte_per_month,
                    months_of_work, individual_count, annual_fte), \(x) as.double(x)))
  

  #assign funding type
  df_hrh_lim <- df_hrh_lim %>% 
    mutate(funding_type = case_when(interaction_type == "Direct Service Delivery" ~ "Service Delivery (SD)",
                                    interaction_type == "Not Specified" ~ interaction_type,
                                    program == "ASP" ~ "Above Site Non-SD",
                                    program %in% c("PM", "Applied Pipeline") ~ program,
                                    interaction_type == "Non-Service Delivery" ~ "Site Level Non-SD")) 
  
  #total staff count
  df_total <- df_hrh_lim %>% 
    group_by(fiscal_year) %>% 
    summarise(across(c(individual_count, annual_fte, actual_annual_spend),
                     \(x) sum(x, na.rm = TRUE)),
              .groups = "drop") %>% 
    mutate(cat = "",
           subcat = "Total Staffing",
           .before = 1)
  
  
  #staff count by SD vs NSD
  df_sd <- df_hrh_lim %>% 
    group_by(fiscal_year, funding_type) %>% 
    summarise(across(c(individual_count, annual_fte, actual_annual_spend),
                     \(x) sum(x, na.rm = TRUE)),
              .groups = "drop") %>% 
    mutate(cat = "Service Delivery Type",
           subcat = funding_type,
           .before = 1) %>% 
    select(-funding_type)
  
  #staff count by work location 
  df_loc <- df_hrh_lim %>% 
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
  df_er <- df_hrh_lim %>% 
    group_by(fiscal_year, er_category) %>% 
    summarise(across(c(individual_count, annual_fte, actual_annual_spend),
                     \(x) sum(x, na.rm = TRUE)),
              .groups = "drop") %>% 
    mutate(cat = "ER Category",
           er_category = recode(er_category,  "Program Management" = "PM"),
           subcat = er_category,
           .before = 1) %>% 
    select(-er_category)
  
  
  
  df_viz <- df_total %>% 
    bind_rows(df_sd, df_loc, df_er) %>%
    mutate(lab_val = ifelse(subcat == "Total Staffing",
                            glue("**{subcat}**<br>FTEs: {number_format(accuracy = .1, scale_cut = cut_short_scale())(individual_count)}<br>Individual Staff: {number_format(accuracy = .1, scale_cut = cut_short_scale())(annual_fte)} | Actual Annual Spend: {number_format(accuracy = .1, prefix = '$', scale_cut = cut_short_scale())(actual_annual_spend)}"),
                            glue("**{subcat}**<br>{number_format(accuracy = .1, scale_cut = cut_short_scale())(individual_count)}<br>{number_format(accuracy = .1, scale_cut = cut_short_scale())(annual_fte)} | {number_format(accuracy = .1, prefix = '$', scale_cut = cut_short_scale())(actual_annual_spend)}")))
    
  df_viz <- df_viz %>% 
    mutate(cat = fct_inorder(cat) %>% fct_rev) %>% 
    arrange(cat, individual_count) %>% 
    mutate(subcat = fct_inorder(subcat)) %>% 
    group_by(cat) %>% 
    mutate(row = row_number())

  df_colors <- tibble(cat = "",
                      row = 1) %>% 
    bind_rows(
      expand_grid(cat = c("ER Category", "Location", "Service Delivery Type"),
                  row = 1:4)) %>% 
    bind_cols(fill_color = c(old_rose,
                             glitr::si_rampr("burnt_siennas", 4, alpha = .6),
                             glitr::si_rampr("moody_blues", 4, alpha = .6),
                             glitr::si_rampr("scooters", 4, alpha = .6))) %>% 
    mutate(cat = fct_inorder(cat),
           text_color = case_when(cat == "" ~ "white", 
                                  row == 4 ~ si_palettes$trolley_greys[1], 
                                  TRUE~ matterhorn))
  
  df_viz <- df_viz %>% 
    left_join(df_colors, by = join_by(cat, row))
  
  
  v1 <- df_viz %>% 
    ggplot(aes(individual_count, cat, fill = fill_color, group = subcat)) +
    geom_col(color = "white") +
    geom_richtext(aes(label = lab_val, color = text_color), position = position_stack(vjust = .5),
                  label.color = NA, fill = NA,
                  family = "Source Sans Pro") +
    facet_wrap(.~fct_rev(cat), scales = "free", ncol = 1) +
    scale_fill_identity() +
    scale_color_identity() +
    labs(x = NULL, y = NULL,
         subtitle = glue("{fy} HRH Staffing Footprint in {cntry} Broken Down By FTEs"),
         caption = glue("Source: FY21-22c SitexIM HRH Structured Dataset (not redacted) | Ref id: {ref_id}")
         ) + 
    si_style_nolines() +
    theme(legend.position = "none",
          axis.text = element_blank(),
          panel.spacing = unit(.1, "picas")) 
  
  si_save("Images/14_hrh-footprint-various_a.png", plot = v1)
  
  
  #top staff by title
  df_viz2 <- df_hrh_lim %>% 
    group_by(fiscal_year, employment_title) %>% 
    summarise(across(c(individual_count, annual_fte, actual_annual_spend),
                     \(x) sum(x, na.rm = TRUE)),
              .groups = "drop") %>% 
    slice_max(order_by = annual_fte, n = 10) %>% 
    mutate(lab_val = ifelse(annual_fte == max(annual_fte),
                                   glue("**{employment_title}**<br>FTEs: {number_format(accuracy = .1, scale_cut = cut_short_scale())(individual_count)} | Staff: {number_format(accuracy = .1, scale_cut = cut_short_scale())(annual_fte)} | Annual Spend: {number_format(accuracy = .1, prefix = '$', scale_cut = cut_short_scale())(actual_annual_spend)}"),
                                   glue("**{employment_title}**<br>{number_format(accuracy = .1, scale_cut = cut_short_scale())(individual_count)} | {number_format(accuracy = .1, scale_cut = cut_short_scale())(annual_fte)} | {number_format(accuracy = .1, prefix = '$', scale_cut = cut_short_scale())(actual_annual_spend)}")))  
   
   v2 <- df_viz2 %>% 
    ggplot(aes(annual_fte, fct_reorder(lab_val, annual_fte))) +
    geom_col(fill = genoa, alpha = .7) +
    scale_x_continuous(expand = c(.005, .005), label = comma) +
    labs(x = NULL, y = NULL,
         subtitle = glue("Top FTEs Position Titles in {cntry}"),
         caption = glue("Source: FY21-22c SitexIM HRH Structured Dataset (not redacted) | Ref id: {ref_id}")) + 
    si_style_xgrid() +
    theme(axis.text.y = element_markdown())
 
   si_save("Images/14_hrh-footprint-various_b.png", plot = v2)
   
  
  v1 + v2 +
    plot_layout(widths = c(2, 1))
  
  si_save("Images/14_hrh-footprint-various_c.png")
  
  
  