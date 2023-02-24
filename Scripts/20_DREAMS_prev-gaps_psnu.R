# PROJECT:  hardapoart
# AUTHOR:   Jessica Hoehner, Baboyma Kagniniwa | USAID
# PURPOSE:  HIV prevalence of AGYW and ABYM ages 10-24 by geography
# REF ID:   70241287 
# LICENSE:  MIT
# DATE:     2023-02-17
# UPDATED: 
# Notes: Developed from image reference id 8fb89847

# # LIBRARIES ----

# Libraries are pre-loaded from 91_setup.R

# # NOTES ----

# Data is pre-loaded 91_setup.R

# prep -------------------------------------------------------------------------
  
prep_hiv_prev_DREAMS <- function(df, cntry,
                                 add_style = T) {
  
  #clean exit if no data
  if(cntry %ni% unique(df$country))
    return(NULL)
  
  ## PSNU/Age/Sex Summaries
  df_pops <- df %>% 
    dplyr::filter(country == cntry, 
                  ageasentered %in% c("10-14", "15-19", "20-24")) %>% 
    dplyr::group_by(fiscal_year, operatingunit, country, 
                    psnuuid, psnu, indicator, ageasentered, sex) %>% 
    dplyr::summarise(value = sum(targets, na.rm = T), .groups = "drop")
  
  #clean exit if no data
  if(nrow(df_pops) == 0)
    return(NULL)
  
  ## Add OU/Country Summary
  
  df_pops <- df_pops %>% 
    group_by(fiscal_year, operatingunit, country, indicator, ageasentered, sex) %>% 
    summarise(value = sum(value, na.rm = T), .groups = "drop") %>% 
    mutate(psnu = "COUNTRY") %>% 
    bind_rows(df_pops, .)
  
  # NOTE - Keep only Country Summaries
  # df_pops <- df_pops %>% 
  #   filter(psnu != "COUNTRY") %>% 
  #   group_by(fiscal_year, operatingunit, indicator, ageasentered, sex) %>% 
  #   summarise(value = sum(value, na.rm = T), .groups = "drop") %>% 
  #   mutate(psnu = "OU") %>% 
  #   bind_rows(df_pops, .)
  
  # PSNU Only Summaries
  df_pops_psnu <- df_pops %>%
    group_by(fiscal_year, operatingunit, country,
              psnuuid, psnu, indicator) %>%
    summarise(value = sum(value, na.rm = T), .groups = "drop")
  
  # Sex Only Summaries
  df_pops_sex <- df_pops %>%
    group_by(fiscal_year, operatingunit, country,
              psnuuid, psnu, indicator, sex) %>%
    summarise(value = sum(value, na.rm = T), .groups = "drop")
  
  ## Compute Prevalence
  df_prev_sex <- df_pops_sex %>% 
    group_by(fiscal_year, operatingunit, country,  psnuuid, psnu, sex) %>% 
    reframe(prevalence = value[indicator == "PLHIV"] / 
              value[indicator == "POP_EST"]) %>% 
    ungroup() 
  
  df_prev_psnu <- df_pops_psnu %>% 
    group_by(fiscal_year, operatingunit, country,  psnuuid, psnu) %>% 
    reframe(psnu_prev = sum(value[indicator == "PLHIV"], na.rm = T) / 
              sum(value[indicator == "POP_EST"], na.rm = T)) %>% 
    ungroup() 
  
  df_prev <- df_prev_sex %>% 
    left_join(df_prev_psnu,
              by = c("fiscal_year","operatingunit", "country", 
                     "snu1uid", "snu1", "psnuuid", "psnu"))
  
  # ## Add SI Style for viz
  if (add_style) {
    
    df_prev_gap <- df_prev %>%
      select(-psnu_prev) %>%
      mutate(sex = tolower(sex)) %>%
      pivot_wider(names_from = sex,
                  values_from = prevalence) %>%
      mutate(color_gap = grey30k)
    
    df_prev <- df_prev %>%
      left_join(df_prev_gap,
                by = c("fiscal_year","operatingunit", "country",
                       "snu1uid", "snu1", "psnuuid", "psnu")) %>%
      mutate(
        color_sex = case_when(
          sex == "Female" ~ moody_blue,
          sex == "Male" ~ genoa,
          TRUE ~ grey30k),
        psnu_label = case_when(
          psnu %in% c("COUNTRY", "OU") ~ paste0("<span style='color:", usaid_black, "'><strong>", psnu, "</strong></span>"),
          TRUE ~ psnu),) %>%
      group_by(operatingunit) %>%
      mutate(
        threshold = case_when(
          psnu_prev < prevalence[psnu == "COUNTRY"] ~ .3,
          TRUE ~ 1
        )
      ) %>%
      ungroup()
  }
  
  return(df_prev)
}
  
# viz --------------------------------------------------------------------------
  
viz_hiv_prev_DREAMS <- function(df, save = F) {
  
  q <- glue::glue("Are there clear HIV prevalence gaps by AGYW/ABYM?") %>% toupper
    
  if(is.null(df) || nrow(df) == 0)
    return(dummy_plot(q))
  
  # OU/Country Reference line
  
  ref_id <- "70241287"
  ref_psnu <- "COUNTRY"
  vrsn <- 1 
  
  # if (all(na.omit(df$country) %in% df$operatingunit)) {
  #   df <- df %>% filter(psnu != "COUNTRY")
  # } else {
  #   ref_psnu <- c("OU", "COUNTRY")
  # }
  
  # Guides
  gap_max <- df %>% 
    filter(psnu %ni% c("COUNTRY", "OU")) %>% 
    pull(prevalence) %>%
    max() %>%
    round(2)
  
  gap_step <- .01
  
  # Control the number of vlines
  if (gap_max > .10) {
    gap_step <- .05
  } else if (gap_max <= .02) {
    gap_step <- .005
  }
  
  # Foot note for reduced datasets
  n_max <- 21
  
  cap_note <- ifelse(nrow(df) > (n_max +1) * 2, "Note: Limited to the largest 20 HIV Prevalence PSNUs\n", "")
  
  # Display only a subset
  df_viz <- df %>% 
    dplyr::slice_max(order_by = psnu_prev, n = n_max * 2) 
  
  if ("COUNTRY" %ni% df_viz$psnu) {
    df_viz <- df %>% 
      filter(psnu == "COUNTRY") %>% 
      bind_rows(df_viz, .)
  }
  
  # Viz
  viz <- df_viz %>% 
    ggplot(aes(x = reorder(psnu, female), 
               y = prevalence,
               fill = color_sex)) +
    geom_hline(yintercept = seq(from = 0, 
                                to = gap_max, 
                                by = gap_step),
               linewidth = .8, linetype = "dashed", color = grey20k) +
    geom_vline(xintercept = ref_psnu,
               linewidth = .8, linetype = "dashed", color = usaid_darkgrey) +
    geom_segment(aes(xend = reorder(psnu, female),
                     y = female, 
                     yend = male,
                     color = color_gap),
                 linewidth = 2) +
    geom_point(shape = 21, size = 5, color = grey10k) +
    facet_wrap(~fiscal_year, nrow = 1, ncol = 3) +
    scale_fill_identity() +
    scale_color_identity() +
    scale_y_continuous(labels = percent, position = "right") +
    coord_flip() +
    labs(x = "", y = "", 
         title = {q},
         subtitle = glue::glue("{toupper(unique(df$country))} - {unique(df$fiscal_year)} HIV Prevalence Gap between <span style='color:{genoa}'>ABYM</span> & <span style='color:{moody_blue}'>AGYW</span> ages 10-24 by PSNU"),
         caption = glue::glue("{cap_note}{metadata_natsubnat$caption} | USAID/OHA/SIEI |  Ref id: {ref_id} v{vrsn}")) +
    si_style_nolines() +
    theme(plot.subtitle = element_markdown(),
          axis.text.y = element_markdown())
  
  print(viz)
  
  if (save) {
    glitr::si_save(
      plot = viz,
      filename = glue::glue("./Graphics/{unique(df$fiscal_year)} - {toupper(unique(df$country))} HIV Prevalence.png"))
  }
}

