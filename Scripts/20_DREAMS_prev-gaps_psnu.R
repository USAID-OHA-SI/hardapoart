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
  
prep_hiv_prev_DREAMS <- function(df, cntry) {
  
  #clean exit if no data
  if(cntry %ni% unique(df$country))
    return(NULL)
  
  ## PSNU/Age/Sex Summaries
  df_pops <- df %>% 
    dplyr::filter(country == cntry, 
                  indicator %in% c("PLHIV", "POP_EST"),
                  ageasentered %in% c("10-14", "15-19", "20-24"))
  
  #clean exit if no data
  if(nrow(df_pops) == 0)
    return(NULL)
  
  #include overall total and aggregate
  df_pops <- df_pops %>%
    dplyr::bind_rows(df_pops %>% 
                       dplyr::mutate(dplyr::across(c(psnu, psnuuid), \(x) x = "OVERALL"))) %>% 
    dplyr::mutate(group = ifelse(sex == "Female", "AGYW", "ABYM")) %>% 
    dplyr::group_by(fiscal_year,  country, 
                    psnuuid, psnu, indicator, group) %>% 
    dplyr::summarise(value = sum(targets, na.rm = T), .groups = "drop")
  
  #pivot wider to calc prevalence
  df_prev <- df_pops %>% 
    tidyr::pivot_wider(names_from = "indicator",
                       names_glue = "{tolower(indicator)}") %>% 
    dplyr::mutate(prevalence = plhiv / pop_est)
  
  #create a psnu level prevalence to order plot on
  df_prev <- df_prev %>% 
    dplyr::group_by(psnuuid) %>% 
    dplyr::mutate(prevalence_psnu = sum(plhiv, na.rm = TRUE) / sum(pop_est, na.rm = TRUE)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(prevalence_order = dplyr::case_when(fiscal_year == max(fiscal_year) ~ prevalence_psnu))
  
  #apply aes for plotting
  df_prev <- df_prev %>% 
    dplyr::mutate(fill_color = ifelse(group == "AGYW", moody_blue, genoa),
                  fill_alpha = ifelse(psnu == "OVERALL", .9, .75),
                  viz_group = glue::glue("{fiscal_year}{psnu}"))
  
  return(df_prev)
}
  
# viz --------------------------------------------------------------------------
  
viz_hiv_prev_DREAMS <- function(df, save = F) {
  
  q <- glue::glue("Are there clear HIV prevalence gaps by AGYW/ABYM?") %>% toupper
    
  if(is.null(df) || nrow(df) == 0)
    return(dummy_plot(q))
  
  # OU/Country Reference line
  
  ref_id <- "70241287"
  ref_psnu <- "OVERALL"
  vrsn <- 1 
  
  # Foot note for reduced datasets
  n_max <- 21
  
  cap_note <- ifelse(nrow(df) > n_max, "Note: Limited to the largest 20 HIV Prevalence PSNUs\n", "")
  
  # Display only a subset
  v_lim_uids <- df %>% 
    dplyr::filter(fiscal_year == max(fiscal_year)) %>% 
    dplyr::distinct(psnu, psnuuid, prevalence_psnu) %>% 
    dplyr::slice_max(order_by = prevalence_psnu, n = 21) %>% 
    dplyr::pull(psnuuid)
  
  df_viz <- df %>% 
    dplyr::filter(psnuuid %in% v_lim_uids) 
  
  if ("OVERALL" %ni% df_viz$psnu) {
    df_viz <- df %>% 
      filter(psnu == "OVERALL") %>% 
      bind_rows(df_viz, .)
  }
  
  # Viz
  viz <- df_viz %>% 
    ggplot(aes(x = prevalence,
               y = forcats::fct_reorder(psnu, prevalence_order, .fun = max, .na_rm = TRUE), 
               color = fill_color,
               alpha = fill_alpha,
               group = viz_group)) +
    geom_vline(xintercept = 0, color = "#D3D3D3") +
    geom_hline(yintercept = ref_psnu,
               linewidth = .8, linetype = "dashed", color = usaid_darkgrey) +
    geom_line(linewidth = 2, alpha = 1, color = "white") +
    geom_line(linewidth = 2, alpha = .6, color = grey30k) +
    geom_errorbar(aes(xmin = prevalence_psnu, xmax = prevalence_psnu), size = 1, color = "white") +
    geom_point(size = 5, color = "white", alpha = 1, na.rm = TRUE) +
    geom_point(size = 5, na.rm = TRUE) +
    facet_wrap(~fiscal_year, nrow = 1, ncol = 3) +
    scale_color_identity() +
    scale_alpha_identity() +
    scale_x_continuous(labels = scales::percent) +
    labs(x = "", y = "",
         title = {q},
         subtitle = glue::glue("{unique(df$country)}} HIV prevalence gap between <span style='color:{genoa}'>ABYM</span> & <span style='color:{moody_blue}'>AGYW</span> ages 10-24"),
         caption = glue::glue("{cap_note}{metadata_natsubnat$caption} | USAID/OHA/SIEI |  Ref id: {ref_id} v{vrsn}")) +
    si_style_nolines() +
    theme(plot.subtitle = element_markdown(),
          axis.text.y = element_markdown())
  
  print(viz)
  
  if (save) {
    glitr::si_save(
      plot = viz,
      filename = glue::glue("./Graphics/{unique(df$fiscal_year)} - {toupper(unique(df$country))} HIV Prevalence AGYW-ABYM.png"))
  }
}

