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
  
  epi_viz <- df %>% 
    filter(indicator %in% c("Number Total Deaths HIV Pop", "Number New HIV Infections"),
           age == "all",
           sex == "all",
           country == cntry) %>% 
    select(c(country, year, indicator, estimate)) %>% 
    spread(indicator, estimate) %>% 
    janitor::clean_names()  %>% 
    mutate(epi_gap = number_new_hiv_infections - number_total_deaths_hiv_pop,
           max_plot_pt = max(number_new_hiv_infections)) %>% 
    rename(cntry = country)
  
  return(epi_viz)
}

#prepare 95s df
prep_95s <- function(df, cntry) {
  
  goal <- 95
 
   #limit Test and Treat data
  df_tt_lim <- df %>% 
    filter(year == max(year),
           indicator %in% c("Percent Known Status of PLHIV",
                            "Percent on ART of PLHIV",
                            "Percent VLS of PLHIV"),
           country == cntry,
    ) %>% 
    select(year, country, indicator, age, sex, estimate) %>% 
    rename(value = estimate) %>% 
    mutate(pop = str_c(sex, age, sep = " ")) %>%
    mutate(pop = recode(pop, "All All" = "All Ages",
                        "All 0-14" = "Children 0-14",
                        "All 15+" = "Adults 15+")) %>% 
    select(-c(sex, age))
  
  #recode indicators and calculate goal
  df_tt_lim <- df_tt_lim %>% 
    filter(!is.na(value)) %>% 
    mutate(indicator = recode(indicator, "Percent Known Status of PLHIV" = "Known\nStatus",
                              "Percent on ART of PLHIV" = "On\nART",
                              "Percent VLS of PLHIV" = "VLS"),
           set = recode(indicator, "Known\nStatus" = 1,
                        "On\nART" = 2,
                        "VLS" = 3),
           goal_rate = round((goal/100)^set*100),
           achv = value > goal_rate) %>% 
    group_by(pop) %>% 
    mutate(gap = goal_rate - value,
           grouping = case_when(max(gap, na.rm = TRUE) <= 0 ~ "Achieved",
                                gap == max(gap, na.rm = TRUE) ~ str_replace(indicator, "\\n", " "),
                                TRUE ~ NA_character_), 
           gap = max(gap)) %>%
    ungroup() %>% 
    mutate(fill_color = case_when(achv == TRUE ~ scooter,
                                  TRUE ~ "white")) 
  
  return(df_tt_lim)
  
}


# VIZ -------------------------------------------------------------

# plot epi curves
viz_epi_control <- function(df) {
  
  clean_number <- function(x, digits = 0){
    dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                     x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                     x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                     TRUE ~ glue("{x}"))
  }
  
  epi_gap_end <- df %>% filter(year == max(year)) %>% pull(epi_gap)
  
  viz_epi <- 
    df %>% 
    ggplot(aes(x = year)) +
    geom_blank(aes(y = max_plot_pt)) +
    geom_blank(aes(y = -max_plot_pt)) +
    geom_area(aes(y = number_new_hiv_infections), fill = "#C6D5E9", alpha = 0.95) +
    geom_area(aes(y = -number_total_deaths_hiv_pop), fill = "#F1CED2",  alpha = 0.95) +
    geom_line(aes(y = number_new_hiv_infections), color = denim, size = 1) +
    geom_line(aes(y = -number_total_deaths_hiv_pop), color = old_rose, size = 1) +
    geom_line(aes(y = epi_gap), color = "white", size = 0.25) +
    geom_text(data = . %>% filter(year == 2020),
              aes(y = epi_gap, label = paste("Epi Gap"),
                  x = year, color = trolley_grey), size = 12 / .pt, family = "Source Sans Pro Light",
              hjust = -0.3, vjust = 0) +
    geom_point(data = . %>% filter(year == max(year)), 
               aes(y = number_new_hiv_infections, fill = denim), shape = 21, color = "white", size = 3)+
    geom_point(data = . %>% filter(year == max(year)), 
               aes(y = -number_total_deaths_hiv_pop, fill = old_rose), shape = 21, color = "white", size = 3) + 
    geom_text(data = . %>% filter(year == max(year)), 
              aes(y = number_new_hiv_infections, color = denim, 
                  label = clean_number(number_new_hiv_infections)),
              hjust = -0.3, size = 12/.pt,
              family = "Source Sans Pro Light") +
    geom_text(data = . %>% filter(year == 2015),
              aes(y = number_new_hiv_infections + 10000, label = paste("Number New Infections"),
                  x = year, color = denim), size = 12 / .pt, family = "Source Sans Pro SemiBold",
              hjust = 0, vjust = 0) +
    geom_text(data = . %>% filter(year == max(year)), 
              aes(y = -number_total_deaths_hiv_pop, color = old_rose, 
                  label = clean_number(number_total_deaths_hiv_pop)),
              hjust = -0.3, size = 12/.pt,
              family = "Source Sans Pro Light") +
    geom_text(data = . %>% filter(year == 2015),
              aes(y = -number_total_deaths_hiv_pop - 10000, label = paste("Total PLHIV Deaths"),
                  x = year, color = old_rose), size = 12 / .pt, family = "Source Sans Pro SemiBold",
              hjust = 0, vjust = 0) +
    scale_fill_identity() +
    scale_color_identity() +
    scale_y_continuous(label = ~ label_number(scale_cut = cut_short_scale())(abs(.))) +
    scale_x_continuous(breaks = seq(1990, 2024, 5)) +
    geom_hline(yintercept = 0, color = grey80k) +
    si_style_ygrid(text_scale = 1.15) +
    labs(x = NULL, y = NULL,
         title = glue("{unique(df$cntry) %>% toupper()}: NUMBER OF <span style= 'color:#2057a7;'> 
  NEW HIV INFECTIONS</span> AND <span style = 'color:#c43d4d;'> TOTAL PLHIV DEATHS </span> AND PROGRESS TO 95S")) +
    coord_cartesian(expand = T, clip = "off") +
    theme(plot.title = element_markdown())
  
  return(viz_epi)
  
}

#plot 95s progress
viz_95s <- function(df) {
  
  ref_id <- "02e4fc9c" 
  
  viz_95 <- df %>% 
    ggplot(aes(indicator, pop,
               fill = fill_color, color = scooter, shape = 22)) +
    geom_point(size = 15) +
    geom_vline(xintercept = 3.5) +
    geom_text(data = . %>% filter(achv != TRUE & indicator != "Epi\nControl"), 
              vjust = .5, hjust = .5,
              aes(label = value), family = "Source Sans Pro SemiBold", size = 6) +
    geom_text(data = . %>% filter(pop == "All Ages"), 
              vjust = -5, hjust = .5,
              aes(label = goal_rate, color = trolley_grey), family = "Source Sans Pro Light", size = 4) +
    geom_text(data = . %>% filter(pop == "All Ages" & indicator == "Known\nStatus"), 
              vjust = -5, hjust = 1.5,
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
         caption =  glue("{metadata_unaids$caption} | Ref id: {ref_id}")) +
    si_style_nolines() +
    theme(axis.text.y = element_markdown(),
          strip.text.y = element_blank(),
          panel.spacing.y = unit(.5, "lines"))
  
  return(viz_95)
  
}

#patchwork all together!
viz_unaids_all <- function(cntry) {
  
  v1 <- prep_epi_control(df_unaids_epi, cntry) %>%
    viz_epi_control()
  
  v2 <- prep_95s(df_unaids_tt, cntry) %>% 
    viz_95s()
  
  viz_final <- v1 + v2 + plot_layout(widths = c(2, 1), heights = c(10))
  
  return(viz_final)
  
}


