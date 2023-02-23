##  PROJECT: hardapoart
##  AUTHOR:  Baboyma Kagniniwa | USAID
##  PURPOSE: HIV Prevalence by geographies and gender
##  REF. ID: 8fb89847
##  LICENCE: MIT
##  DATE:    2023-02-02
##  UPDATE:  2023-02-13

# # LIBRARIES ----
  
  # Libraries are pre-loaded  
  
# # NOTES ----
  
  # Data is pre-loaded
  
# FUNCTIONS ----

  #' @title Prep HIV Prevalence Source Data
  #' 
  #' @param cntry     OU/Country name
  #' @param add_style Append color code
  #' 
  prep_hiv_prevalence <- function(df, cntry,
                                  add_style = T) {
    
    #clean exit if no data
    if(cntry %ni% unique(df$country))
      return(NULL)
    
    ## PSNU/Age/Sex Summaries
    df_pops <- df %>% 
      dplyr::filter(fiscal_year == max(fiscal_year),
                    country == cntry) %>% 
      dplyr::group_by(fiscal_year, operatingunit, country, snu1uid, snu1,
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
               snu1uid, snu1, psnuuid, psnu, indicator) %>%
      summarise(value = sum(value, na.rm = T), .groups = "drop")

    # Sex Only Summaries
    df_pops_sex <- df_pops %>%
      group_by(fiscal_year, operatingunit, country,
               snu1uid, snu1, psnuuid, psnu, indicator, sex) %>%
      summarise(value = sum(value, na.rm = T), .groups = "drop")
    
    ## Compute Prevalence
    df_prev_sex <- df_pops_sex %>% 
      group_by(fiscal_year, operatingunit, country, snu1uid, snu1, psnuuid, psnu, sex) %>% 
      reframe(prevalence = value[indicator == "PLHIV"] / 
                value[indicator == "POP_EST"]) %>% 
      ungroup() 
    
    df_prev_psnu <- df_pops_psnu %>% 
      group_by(fiscal_year, operatingunit, country, snu1uid, snu1, psnuuid, psnu) %>% 
      reframe(psnu_prev = sum(value[indicator == "PLHIV"], na.rm = T) / 
                sum(value[indicator == "POP_EST"], na.rm = T)) %>% 
      ungroup() 
    
    df_prev <- df_prev_sex %>% 
      left_join(df_prev_psnu,
                by = c("fiscal_year","operatingunit", "country", 
                       "snu1uid", "snu1", "psnuuid", "psnu"))
    
    ## Add SI Style for viz
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
            TRUE ~ grey30k
          ),
          psnu_label = case_when(
            psnu %in% c("COUNTRY", "OU") ~ paste0("<span style='color:", usaid_black, "'><strong>", psnu, "</strong></span>"),
            TRUE ~ psnu
          ),
        ) %>% 
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
  
  
  #' @title Visualize HIV Prevalence by PSNU/Gender
  #' 
  #' 
  #' 
  viz_hiv_prevalence <- function(df, save = F) {
    
    q <- glue::glue("Are there clear HIV prevalence gaps by PSNU or sex?") %>% toupper
      
    if(is.null(df) || nrow(df) == 0)
      return(dummy_plot(q))
    
    # OU/Country Reference line
    
    ref_id <- "8fb89847"
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
      scale_fill_identity() +
      scale_color_identity() +
      scale_y_continuous(labels = percent, position = "right") +
      coord_flip() +
      labs(x = "", y = "", 
           title = {q},
           subtitle = glue::glue("{toupper(unique(df$country))} - {unique(df$fiscal_year)} HIV Prevalence Gap between <span style='color:{genoa}'>Male</span> & <span style='color:{moody_blue}'>Female</span> by PSNU"),
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
  
# DATA IMPORT ----
  
  # PEPFAR Nat-Subnat MSD
  
# MUNGING ----
  
  # Test Prep
  
  # df_prev <- pepfar_country_list %>% 
  #   pull(country) %>% 
  #   nth(26) %>% 
  #   prep_hiv_prevalence(df = df_natsubnat,
  #                       cntry = .,
  #                       add_style = T)
  
# VIZ ----

  # Test Viz
  
  # pepfar_country_list %>%
  #   pull(country) %>%
  #   #first() %>%
  #   nth(26) %>%
  #   #nth(28) %>%
  #   #nth(46) %>%
  #   prep_hiv_prevalence(df = df_natsubnat,
  #                       cntry = .,
  #                       add_style = T) %>%
  #   viz_hiv_prevalence()

  
# EXPORT ----
  
