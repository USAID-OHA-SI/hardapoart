# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  
# REF ID:   cc4f6cf7 
# LICENSE:  MIT
# DATE:     2023-02-07
# UPDATED: 
  

# MUNGE -------------------------------------------------------------------
 
#grab undiagnosed PLHIV from UNAIDS
  #NUmber PLHIV - Number who know status
prep_undiagnosed <- function(cntry) {
  
  #clean exit if no data
  if(cntry %ni% unique(df_unaids_tt$country))
    return(NULL)
  
  clean_number <- function(x, digits = 0){
    dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                     x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                     x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                     TRUE ~ glue("{x}"))
  }
  
   
   #grab undiagnosed from UNAIDS Data
   df_undiagnosed <- df_unaids_tt %>% 
     dplyr::bind_rows(df_epi_original) %>%
     dplyr::filter(indicator %in% c("Number Known Status of PLHIV",
                             "Number PLHIV"),
            country == cntry,
            year == max(year),
            sex == "All") %>% 
     dplyr::select(year, country, indicator, age, sex, estimate) %>% 
     dplyr::rename(value = estimate) %>% 
     dplyr::mutate(pop = str_c(sex, age, sep = " ")) %>%
     dplyr::mutate(pop = dplyr::recode(pop, "All All" = "All Ages",
                         "All 0-14" = "<15",
                         "All 15+" = "15+")) %>% 
     dplyr::select(-c(sex, age)) %>% 
     tidyr::pivot_wider(names_from = "indicator", values_from = "value") %>% 
     janitor::clean_names() %>% 
     dplyr::mutate(undiagnosed_pop = number_plhiv - number_known_status_of_plhiv) %>%
     dplyr::mutate(age_facet = glue::glue("{pop}: ~{clean_number(undiagnosed_pop)} Undiagnosed PLHIV")) %>% 
     dplyr::select(year, country, pop, undiagnosed_pop, age_facet)
   
   return(df_undiagnosed)
   
 }
  
 #munge modalities into case-finding and prevention (prevention = PMTCT, ANC, VMMC)
 prep_modality_age <- function(df, cntry, agency, ...){   
   
   #clean exit if no data
   if(cntry %ni% unique(df$country) | agency %ni% unique(df$funding_agency))
     return(NULL)
   
   df_hts_full <- df %>% 
     dplyr::filter(indicator == "HTS_TST",
            country == cntry,
            standardizeddisaggregate == "Modality/Age/Sex/Result",
            fiscal_year <= metadata_msd$curr_fy,
            funding_agency == agency) %>%  
     dplyr::mutate(mod_type = case_when(
       stringr::str_detect(modality, "PMTCT ANC") ~ "Prevention",
       stringr::str_detect(modality, "Post ANC1") ~ "Prevention",
       stringr::str_detect(modality, "VMMC") ~ "Prevention",
       #modality == "VCT" ~ "VCT",
     #  str_detect(modality, "SNSMod") ~ "Community SNS",
       TRUE ~ "Case Finding")
     ) %>%
     dplyr::group_by(fiscal_year, funding_agency, mod_type, ...) %>%
     dplyr::summarise(across(starts_with("qtr"), sum, na.rm = TRUE),
                      .groups = "drop") %>%
     gophr::reshape_msd() %>%
     dplyr::select(-period_type) %>%
     dplyr::group_by(period, ...) %>%
     dplyr::mutate(contribution = value/sum(value)) %>%
     dplyr::ungroup() %>%
     dplyr::mutate(start = dplyr::case_when(period == min(period) ~ contribution),
                  end = dplyr::case_when(period == max(period) ~ contribution)) %>%
     dplyr::mutate(mod_order = forcats::fct_reorder(mod_type, value, .desc = T)) %>% 
     tidyr::complete(mod_type) %>% 
     dplyr::group_by(mod_type, ...) %>% 
     tidyr::fill(mod_order, .direction = "up") %>% 
     dplyr::group_by(period, ...) %>% 
     dplyr::mutate(pd_tot = sum(value, na.rm = T), 
            pd_25 = pd_tot * 0.25, 
            pd_50 = pd_tot * 0.5,
            pd_75 = pd_tot * 0.75) %>% 
     dplyr::ungroup() %>% 
     dplyr::mutate(mod_color = dplyr::case_when(
       mod_type == "Prevention" ~ scooter, 
       mod_type == "Case Finding" ~ golden_sand,
      # mod_type == "Other PITC" ~ "#AF6458",
       #mod_type == "PMTCT"~ "#736F4C",
       #mod_type == "Community SNS" ~ "#526A83",
       TRUE ~ "#7C7C7C"
     ),
     note = dplyr::case_when(
       mod_type == "Case Finding" & period == "FY20Q1" ~ "HTS_TST_POS",
       TRUE ~ NA_character_
     )) %>% 
     dplyr::filter(!is.na(mod_order))
   
   df_hts_full <- df_hts_full %>% 
     dplyr::left_join(prep_undiagnosed(cntry), by = c("trendscoarse" = "pop"))
   
   return(df_hts_full)
 } 


# VIZ -----------------------------------------------------------------------
 
 viz_modality_age <- function(df) {
   
   if(is.null(df))
     return(print(paste("No data available.")))
   
   ref_id <- "cc4f6cf7"
   
   df %>% 
     dplyr::filter(trendscoarse != "Unknown Age") %>% 
     ggplot2::ggplot(aes(x = period)) +
     ggplot2::geom_col(aes(y = pd_tot), fill = glitr::trolley_grey_light) +
     ggplot2::geom_col(aes(y = value, fill = mod_color)) +
     ggplot2::geom_text(data = . %>% filter(trendscoarse == "15+", period == min(period)),
               aes(y = pd_tot, label = paste("Total\nTests")), color = glitr::trolley_grey,
               size = 9 / .pt, family = "Source Sans Pro", vjust = 1.2) +
     ggplot2::geom_errorbar(aes(ymin = pd_25, ymax = pd_25), 
                   size = 0.25, color = "white", 
                   linetype = "dotted") +
     ggplot2::geom_errorbar(aes(ymin = pd_50, ymax = pd_50), 
                   size = 0.25, color = "white", 
                   linetype = "dotted") +
     geom_errorbar(aes(ymin = pd_75, ymax = pd_75), 
                   size = 0.25, color = "white", 
                   linetype = "dotted") +
     ggplot2::scale_fill_identity() +
     ggplot2::facet_grid(mod_order ~ forcats::fct_reorder(age_facet, value, na.rm = TRUE, .desc = TRUE), scales = "free_y") +
     ggplot2::geom_text(aes(y = value, label = scales::percent(start, 1)), size = 7/.pt, vjust = -0.5, family = "Source Sans Pro") +
     ggplot2::geom_text(aes(y = value, label = scales::percent(end, 1)), size = 7/.pt,  vjust = -0.5, family = "Source Sans Pro") +
     ggplot2::geom_text(data = . %>% dplyr::filter(trendscoarse == "15+", period == min(period)),
               aes(y = value, label = paste("modality\nshare")),
               size = 6.5/.pt, vjust = 1.5, family = "Source Sans Pro", color = "white") +
     # geom_text(aes(y = pd_tot, label = note), size = 8/.pt, color = "#7C7C7C",
     #           hjust = 0.2, vjust = -0.25) +
     ggplot2::labs(x = NULL, y = NULL,
          title = glue::glue("PEPFAR/{unique(df$country) %>% toupper} HTS_TST MODALITY SUMMARY BY AGE"),
          caption = glue::glue("Note: Undiagnosed PLHIV proxy calculated as the number of PLHIV minus PLHIV who know their status (UNAIDS 2022 Data)
                               Source: UNAIDS 2022 Epidemiology Data & {metadata_msd$source} | USAID | Ref id:{ref_id}")) +
     ggplot2::theme(legend.position = "none") +
     ggplot2::scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
     ggplot2::scale_x_discrete(labels = c("FY20Q1", "", "", "",
                                 "FY21Q1", "", "", "",
                                 "FY22Q1", "", "", "")) +
     glitr::si_style_ygrid(facet_space = 0.5)   
   
 }
 
 # #BY SNU (revisit?) -----------------------------------------
 # 
 # df_modality_snu <- munge_modality(df_msd, cntry, snu1)
 # 
 # top_snu <- df_modality_snu %>% 
 #   filter(period == max(period)) %>% 
 #   arrange(desc(value)) %>% 
 #   slice_head() %>% 
 #   pull(snu1)
 # 
 # df_modality_snu %>% 
 #   filter(period == max(period)) %>% 
 #   ggplot(aes(x = period)) +
 #   geom_col(aes(y = pd_tot), fill = trolley_grey_light) +
 #   geom_col(aes(y = value, fill = mod_color)) +
 #   geom_text(data = . %>% filter(snu1 == top_snu, period == min(period)),
 #             aes(y = pd_tot, label = paste("Total\nTests")), color = trolley_grey,
 #             size = 9 / .pt, family = "Source Sans Pro", vjust = 1.2) +
 #   geom_errorbar(aes(ymin = pd_25, ymax = pd_25), 
 #                 size = 0.25, color = "white", 
 #                 linetype = "dotted") +
 #   geom_errorbar(aes(ymin = pd_50, ymax = pd_50), 
 #                 size = 0.25, color = "white", 
 #                 linetype = "dotted") +
 #   geom_errorbar(aes(ymin = pd_75, ymax = pd_75), 
 #                 size = 0.25, color = "white", 
 #                 linetype = "dotted") +
 #   scale_fill_identity() +
 #   geom_text(data = . %>% filter(snu1 == top_snu, period == min(period)),
 #             aes(y = value, label = paste("modality\nshare")),
 #             size = 6.5/.pt, vjust = 1.5, family = "Source Sans Pro", color = "white") +
 #   facet_grid(mod_order ~ fct_reorder(snu1, value, na.rm = TRUE, .desc = TRUE), scales = "free_y") +
 #   geom_text(aes(y = value, label = percent(start, 1)), size = 7/.pt, vjust = -0.5) +
 #   geom_text(aes(y = value, label = percent(end, 1)), size = 7/.pt,  vjust = -0.5) +
 #   # geom_text(aes(y = pd_tot, label = note), size = 8/.pt, color = "#7C7C7C",
 #   #           hjust = 0.2, vjust = -0.25) +
 #   labs(x = NULL, y = NULL,
 #        title = glue("{cntry %>% toupper} HTS_TST MODALITY SUMMARY BY SNU IN {metadata_msd$curr_pd}"),
 #        caption = glue("Source: UNAIDS 2022 Epidemiology Data & {metadata_msd$source} | Ref id:{ref_id}")) +
 #   theme(legend.position = "none") +
 #   scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
 #   # scale_x_discrete(labels = c("FY20Q1", "", "", "",
 #   #                             "FY21Q1", "", "", "",
 #   #                             "FY22Q1", "", "")) +
 #   si_style_ygrid(facet_space = 0.5)+
 #   theme(axis.text.x = element_blank())
 # 
 # si_save("Images/05_testing_snu.png")
 
 