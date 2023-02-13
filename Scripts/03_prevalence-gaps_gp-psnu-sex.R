##  PROJECT: <Project covered by this repository>
##  AUTHOR:  Baboyma Kagniniwa | USAID
##  PURPOSE: HIV Prevalence by geographies and gender
##  REF. ID: 8fb89847
##  LICENCE: MIT
##  DATE:    2023-02-02
##  UPDATE:  2023-02-02

# # LIBRARIES ----
#   
#   library(tidyverse)
#   library(glamr)
#   library(gophr)
#   library(grabr)
#   library(mindthegap)
#   library(glitr)
#   library(extrafont)
#   library(scales)
#   library(tidytext)
#   library(ggtext)
#   library(patchwork)
#   
# # NOTES ----
# 
# # DISCLAIMERS ----
# 
# # GLOBAL PARAMS ----
#   
#   ## Script Reference
#   ref_id <- "8fb89847"
#   
#   # SI Backstop Coverage
#   cntry = "Nigeria"
  
  ## Dirs 
  
  # dir_mer <- si_path(type = "path_msd")
  # dir_out <- "./Dataout"
  # dir_imagess <- "./Images"
  # dir_graphics <- "./Graphics"
  
  ## Files
  
  # file_subnat <- dir_mer %>% 
  #   return_latest("NAT_SUBNAT")
  # 
  # file_psnu <- dir_mer %>% 
  #   return_latest("PSNU_IM")
  
  ## Info
  
  # src_msd <- source_info(file_psnu)
  # 
  # curr_fy <- source_info(file_psnu, return = "fiscal_year")
  # curr_qtr <- source_info(file_psnu, return = "quarter")
  # curr_pd <- source_info(file_psnu, return = "period")
  
  ## Tech Areas / Disaggs
  
  # inds_pops <- c("POP_EST", "PLHIV")
  # disaggs_pops <- c("Age/Sex", "Age/Sex/HIVStatus")
  
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
      dplyr::summarise(value = sum(targets, na.rm = T), .groups = "drop") %>% 
      gophr::clean_psnu()
    
    ## Add OU/Country Summary
    
    df_pops <- df_pops %>% 
      group_by(fiscal_year, operatingunit, country, indicator, ageasentered, sex) %>% 
      summarise(value = sum(value, na.rm = T), .groups = "drop") %>% 
      mutate(psnu = "COUNTRY") %>% 
      bind_rows(df_pops, .)
    
    df_pops <- df_pops %>% 
      filter(psnu != "COUNTRY") %>% 
      group_by(fiscal_year, operatingunit, indicator, ageasentered, sex) %>% 
      summarise(value = sum(value, na.rm = T), .groups = "drop") %>% 
      mutate(psnu = "OU") %>% 
      bind_rows(df_pops, .)
    
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
            psnu_prev < prevalence[psnu == "OU"] ~ .3,
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
  viz_hiv_prevalence <- function(df,
                                 save = F) {
    
    if(is.null(df))
      return(print(paste("No data available.")))
    
    # OU/Country Reference line
    
    ref_id <- "8fb89847"
    ref_psnu <- "OU"
    vrsn <- 1 
    
    if (all(na.omit(df$country) %in% df$operatingunit)) {
      df <- df %>% filter(psnu != "COUNTRY")
    } else {
      ref_psnu <- c("OU", "COUNTRY")
    }
    
    # Guides
    gap_max <- df %>% 
      filter(psnu != "COUNTRY" & psnu != "OU") %>% 
      pull(prevalence) %>%
      max() %>%
      round(2)
    
    gap_step <- .01
    
    if (gap_max > .01) {
      gap_step <- .05
    }
    
    # Viz
    viz <- df %>% 
      ggplot(aes(x = reorder(psnu, female), 
                 y = prevalence,
                 fill = color_sex)) +
      geom_hline(yintercept = seq(from = 0, 
                                  to = gap_max, 
                                  by = gap_step),
                 size = .8, linetype = "dashed", color = grey20k) +
      geom_vline(xintercept = ref_psnu,
                 size = .8, linetype = "dashed", color = usaid_darkgrey) +
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
           title = glue::glue("{toupper(unique(df$country))} - {unique(df$fiscal_year)} HIV PREVALANCE"),
           subtitle = glue::glue("HIV Prevalence Gap between <span style='color:{genoa}'>Male</span> & <span style='color:{moody_blue}'>Female</span> by PSNU"),
           caption = glue::glue("{metadata_natsubnat$caption} | USAID | Ref id: {ref_id} v{vrsn}")) +
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
  
  # PEPFAR Program Data
  
  #df_subnat <- file_subnat %>% read_msd()
  
# MUNGING ----
  
  # Test Munge
  
  # df_prev <- prep_hiv_prevalence(df = df_natsubnat, 
  #                                 cntry = cntry,
  #                                 fy = metadata_natsubnat$curr_fy,
  #                                 add_style = T)
  
# VIZ ----

  # Test Viz
  
  # viz_hiv_prevalence(df = df_prev, cntry = cntry,
  #                    fy = metadata_natsubnat$curr_fy,
  #                    pd = metadata_natsubnat$curr_pd,
  #                    src = metadata_natsubnat$source,
  #                    rid = ref_id)

  
# EXPORT ----
  