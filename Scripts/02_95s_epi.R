# PROJECT:  hardapoart
# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  overview of 95s and epi control
# REF ID:   02e4fc9c 
# LICENSE:  MIT
# DATE:     2023-02-02
# UPDATED: 
# NOTE:     based on agitprop/blob/main/Scripts/02_epi_ann_unaids-global-epi-control.R
#           and catch-22/Scripts/2021_12_Call To IP_OU/ctip-ou-unaids_plus_epi.R



# MUNGE -------------------------------------------------------------------

# prepare epi control df
prep_epi_control <- function(df, cntry) {
  
  #clean exit if no data
  if(cntry %ni% unique(df$country))
    return(NULL)
  
  epi_viz <- df %>% 
    dplyr::filter(indicator %in% c("Number Total Deaths HIV Pop", "Number New HIV Infections"),
                  age == "all",
                  sex == "all",
                  country == cntry) 
  
  if(nrow(epi_viz) == 0)
    return(NULL)
  
  epi_viz <- epi_viz %>% 
    dplyr::select(c(country, year, indicator, estimate)) %>% 
    tidyr::spread(indicator, estimate) %>% 
    janitor::clean_names()  
  
  if("number_new_hiv_infections" %ni% names(epi_viz) || "number_total_deaths_hiv_pop" %ni% names(epi_viz))
    return(NULL)
  
  epi_viz <- epi_viz %>% 
    dplyr::mutate(epi_gap = number_new_hiv_infections - number_total_deaths_hiv_pop,
                  max_plot_pt = max(number_new_hiv_infections)) %>% 
    dplyr::rename(cntry = country)
  
  return(epi_viz)
}

#prepare 95s df
prep_95s <- function(df, cntry) {
  
  #clean exit if no data
  if(cntry %ni% unique(df$country))
    return(NULL)
  
  goal <- 95
  
  #limit Test and Treat data
  df_tt_lim <- df %>% 
    dplyr::filter(year == max(year),
                  indicator %in% c("Percent Known Status of PLHIV",
                                   "Percent on ART of PLHIV",
                                   "Percent VLS of PLHIV"),
                  country == cntry,
    ) 
  
  if(nrow(df_tt_lim) == 0 || sum(df_tt_lim$estimate, na.rm = TRUE) == 0)
    return(NULL)
  
  df_tt_lim <- df_tt_lim %>% 
    dplyr::select(year, country, indicator, age, sex, estimate) %>% 
    dplyr::rename(value = estimate) %>% 
    dplyr::mutate(pop = stringr::str_c(sex, age, sep = " ")) %>%
    dplyr::mutate(pop = dplyr::recode(pop, "All All" = "All Ages",
                                      "All 0-14" = "Children 0-14",
                                      "All 15+" = "Adults 15+")) %>% 
    dplyr::select(-c(sex, age))
  
  #recode indicators and calculate goal
  df_tt_lim <- df_tt_lim %>% 
    dplyr::filter(!is.na(value)) %>% 
    dplyr::mutate(indicator = dplyr::recode(indicator, "Percent Known Status of PLHIV" = "Known\nStatus",
                                            "Percent on ART of PLHIV" = "On\nART",
                                            "Percent VLS of PLHIV" = "VLS"),
                  set = dplyr::recode(indicator, "Known\nStatus" = 1,
                                      "On\nART" = 2,
                                      "VLS" = 3),
                  goal_rate = round((goal/100)^set*100),
                  achv = value > goal_rate) %>% 
    dplyr::group_by(pop) %>% 
    dplyr::mutate(gap = goal_rate - value,
                  grouping = dplyr::case_when(max(gap, na.rm = TRUE) <= 0 ~ "Achieved",
                                              gap == max(gap, na.rm = TRUE) ~ stringr::str_replace(indicator, "\\n", " "),
                                              TRUE ~ NA_character_), 
                  gap = max(gap)) %>%
    dplyr::ungroup() %>% 
    dplyr::mutate(fill_color = dplyr::case_when(achv == TRUE ~ scooter,
                                                TRUE ~ "white")) 
  
  return(df_tt_lim)
  
}


# VIZ -------------------------------------------------------------

# plot epi curves
viz_epi_control <- function(df) {
  
  if(is.null(df) || nrow(df) == 0)
    return(NULL)
  
  clean_number <- function(x, digits = 0){
    dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                     x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                     x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                     TRUE ~ glue("{x}"))
  }
  
  epi_gap_end <- df %>%
    dplyr::filter(year == max(year))%>% 
    dplyr::pull(epi_gap)
  
  viz_epi <- 
    df %>% 
    ggplot2::ggplot(aes(x = year)) +
    ggplot2::geom_blank(aes(y = max_plot_pt)) +
    ggplot2::geom_blank(aes(y = -max_plot_pt)) +
    ggplot2::geom_area(aes(y = number_new_hiv_infections), fill = "#C6D5E9", alpha = 0.95) +
    ggplot2::geom_area(aes(y = -number_total_deaths_hiv_pop), fill = "#F1CED2",  alpha = 0.95) +
    ggplot2::geom_line(aes(y = number_new_hiv_infections), color = glitr::denim, linewidth = 1) +
    ggplot2::geom_line(aes(y = -number_total_deaths_hiv_pop), color = glitr::old_rose, linewidth = 1) +
    ggplot2::geom_line(aes(y = epi_gap), color = "white", linewidth = 0.25) +
    ggplot2::geom_text(data = . %>% dplyr::filter(year == 2020),
                       aes(y = epi_gap, label = paste("Epi Gap"),
                           x = year, color = glitr::trolley_grey), size = 12 / .pt, family = "Source Sans Pro Light",
                       hjust = -0.3, vjust = 0) +
    ggplot2::geom_point(data = . %>% dplyr::filter(year == max(year)), 
                        aes(y = number_new_hiv_infections, fill = glitr::denim), shape = 21, color = "white", size = 3)+
    ggplot2::geom_point(data = . %>% dplyr::filter(year == max(year)), 
                        aes(y = -number_total_deaths_hiv_pop, fill = glitr::old_rose), shape = 21, color = "white", size = 3) + 
    ggplot2::geom_text(data = . %>% dplyr::filter(year == max(year)), 
                       aes(y = number_new_hiv_infections, color = glitr::denim, 
                           label = clean_number(number_new_hiv_infections)),
                       hjust = -0.3, size = 12/.pt,
                       family = "Source Sans Pro Light") +
    ggplot2::geom_text(data = . %>% dplyr::filter(year == 2015),
                       aes(y = number_new_hiv_infections + 50000, label = paste("Number New Infections"),
                           x = year, color = glitr::denim), size = 12 / .pt, family = "Source Sans Pro SemiBold",
                       hjust = 1, vjust = -1) +
    ggplot2::geom_text(data = . %>% dplyr::filter(year == max(year)), 
                       aes(y = -number_total_deaths_hiv_pop, color = glitr::old_rose, 
                           label = clean_number(number_total_deaths_hiv_pop)),
                       hjust = -0.3, size = 12/.pt,
                       family = "Source Sans Pro Light") +
    ggplot2::geom_text(data = . %>% dplyr::filter(year == 2015),
                       aes(y = -number_total_deaths_hiv_pop - 50000, label = paste("Total PLHIV Deaths"),
                           x = year, color = glitr::old_rose), size = 12 / .pt, family = "Source Sans Pro SemiBold",
                       hjust = 1, vjust = 1) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_color_identity() +
    ggplot2::scale_y_continuous(label = ~ scales::label_number(scale_cut = cut_short_scale())(abs(.))) +
    ggplot2::scale_x_continuous(breaks = seq(1990, 2024, 5)) +
    ggplot2::geom_hline(yintercept = 0, color = grey80k) +
    glitr::si_style_ygrid(text_scale = 1.15) +
    ggplot2::labs(x = NULL, y = NULL) +
    #        title = glue::glue("{unique(df$cntry) %>% toupper()}: NUMBER OF <span style= 'color:#2057a7;'> 
    # NEW HIV INFECTIONS</span> AND <span style = 'color:#c43d4d;'> TOTAL PLHIV DEATHS </span> AND PROGRESS TO <span style = 'color:#1e87a5;'>95S</span>")) +
    ggplot2::coord_cartesian(expand = T, clip = "off") +
    ggplot2::theme(plot.title = ggtext::element_markdown())
  
  return(viz_epi)
  
}

#plot 95s progress
viz_95s <- function(df) {
  
  if(is.null(df) || nrow(df) == 0)
    return(NULL)
  
  ref_id <- "02e4fc9c" 
  vrsn <- 2 
  
  viz_95 <- df %>% 
    ggplot(aes(indicator, pop,
               fill = fill_color, color = scooter, shape = 22)) +
    geom_point(size = 15) +
    geom_vline(xintercept = 3.5) +
    geom_text(data = . %>% filter(achv != TRUE & indicator != "Epi\nControl"), 
              vjust = .5, hjust = .5,
              aes(label = value), family = "Source Sans Pro SemiBold", size = 6) +
    geom_text(data = . %>% filter(pop == "All Ages"), 
              vjust = -3, hjust = .5,
              aes(label = goal_rate, color = trolley_grey), family = "Source Sans Pro Light", size = 4) +
    geom_text(data = . %>% filter(pop == "All Ages" & indicator == "Known\nStatus"), 
              vjust = -3, hjust = 1.5,
              aes(label = paste("Goal Rate"), color = trolley_grey), family = "Source Sans Pro Light", size = 4) +
    geom_text(data = . %>% filter(achv == TRUE & indicator != "Epi\nControl"), 
              vjust = .5, hjust = .5,
              aes(label = value), color = "white", family = "Source Sans Pro SemiBold", size = 6) +
    # geom_text(data = . %>% filter(achv != TRUE & indicator == "Epi\nControl"), 
    #           vjust = .5, hjust = .5,
    #           aes(label = value), family = "Source Sans Pro SemiBold", size = 2.5) +
    # geom_text(data = . %>% filter(achv == TRUE & indicator == "Epi\nControl"), 
    #           vjust = .5, hjust = .5,
    #           aes(label = value), color = "white", family = "Source Sans Pro SemiBold", size = 2.5) +
    # geom_text(size = 3, nudge_x = 1, na.rm = TRUE,
    #           aes(label = lab_epi), color = matterhorn, family = "Source Sans Pro") +
    #  facet_grid(grouping~., scales = "free_y", space = "free_y") +
    scale_fill_identity(aesthetics = c("fill", "color")) +
    scale_shape_identity() +
    scale_x_discrete(position = "top", expand = c(.05, .05)) +
    scale_y_discrete(limits = c( "Male 15+", "Female 15+","Adults 15+", "Children 0-14", "All Ages")) +
    #scale_y_reordered() +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL,
         # caption =  glue("{metadata_unaids$caption} | USAID/OHA/SIEI | Ref id: {ref_id} v{vrsn}")
         ) +
    si_style_nolines() +
    theme(axis.text.y = ggtext::element_markdown(),
          strip.text.y = ggplot2::element_blank(),
          panel.spacing.y = unit(.5, "lines"))
  
  return(viz_95)
  
}

#patchwork all together!
viz_unaids_all <- function(cntry) {
  
  ref_id <- "02e4fc9c" 
  vrsn <- 2 
  
  v1 <- prep_epi_control(df_unaids_epi, cntry) %>%
    viz_epi_control()
  
  v2 <- prep_95s(df_unaids_tt, cntry) %>% 
    viz_95s()
  
  if(is.null(v1) && is.null(v2)){
    print(paste("No data available."))
  } else if(is.null(v1)){
    v2
  } else if(is.null(v2)){
    v1
  } else {
    #v1 + v2 + plot_layout(widths = c(2, 1), heights = c(10))

      viz <- v1 + v2 + plot_layout(widths = c(2, 1), heights = c(10)) +
      patchwork::plot_annotation(
        title = glue::glue("{cntry %>% toupper()}: NUMBER OF <span style= 'color:#2057a7;'> 
  NEW HIV INFECTIONS</span> AND <span style = 'color:#c43d4d;'> TOTAL PLHIV DEATHS </span> AND PROGRESS TO <span style = 'color:#1e87a5;'>95S</span>"),
        #subtitle = "Facilities location data availability",
        caption =  glue("{metadata_unaids$caption} | USAID/OHA/SIEI | Ref id: {ref_id} v{vrsn}"),
        theme = si_style_nolines() + ggplot2::theme(
          plot.title = ggtext::element_markdown(),
          plot.subtitle = element_text(hjust = .5)))
      
    return(viz)

  }
  
  
}


