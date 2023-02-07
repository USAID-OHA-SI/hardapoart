# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  
# REF ID:   cc4f6cf7 
# LICENSE:  MIT
# DATE:     2023-02-07
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
library(googlesheets4)
library(mindthegap)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "cc4f6cf7"
  
  cntry <- "Zambia"
  
  # SI specific paths/functions  
  merdata <- file.path(glamr::si_path("path_msd"))
  
  #change this later - this is a zam specific file
  file_path <- return_latest(folderpath = merdata,
                             pattern = "PSNU_IM_FY20-23.*|Zambia.txt")
  
  get_metadata(file_path, caption_note = "Source: UNAIDS 2022 Data on HIV epidemiology")
  
  clean_number <- function(x, digits = 0){
    dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                     x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                     x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                     TRUE ~ glue("{x}"))
  }
  

# IMPORT ------------------------------------------------------------------
  
  #grab unaids data
  df_tt <- pull_unaids(TRUE, "HIV Test & Treat", pepfar_only = TRUE)
  df_epi <- pull_unaids(TRUE, "HIV Estimates", pepfar_only = TRUE)
  

 df_msd <- read_msd(file_path)
  
  

# MUNGE -------------------------------------------------------------------
  
 #grab undiagnosed from UNAIDS Data
  df_undiagnosed <- df_tt %>% 
    bind_rows(df_epi) %>%
    filter(indicator %in% c("Number Known Status of PLHIV",
                            "Number PLHIV"),
           country == cntry,
           year == max(year),
           sex == "All") %>% 
    select(year, country, indicator, age, sex, estimate) %>% 
    rename(value = estimate) %>% 
    mutate(pop = str_c(sex, age, sep = " ")) %>%
    mutate(pop = recode(pop, "All All" = "All Ages",
                        "All 0-14" = "<15",
                        "All 15+" = "15+")) %>% 
    select(-c(sex, age)) %>% 
    pivot_wider(names_from = "indicator", values_from = "value") %>% 
    janitor::clean_names() %>% 
    mutate(undiagnosed_pop = number_plhiv - number_known_status_of_plhiv) %>%
    mutate(age_facet = glue("{pop}: ~{clean_number(undiagnosed_pop)} Undiagnosed PLHIV")) %>% 
   select(year, country, pop, undiagnosed_pop, age_facet)
 
 munge_modality <- function(df, ...){   
   df_hts_full <- df %>% 
     filter(indicator == "HTS_TST",
            standardizeddisaggregate == "Modality/Age/Sex/Result",
            fiscal_year <= metadata$curr_fy) %>%  
     #funding_agency == "USAID", ...) %>% 
     mutate(mod_type = case_when(
       str_detect(modality, "PMTCT ANC") ~ "Prevention",
       str_detect(modality, "Post ANC1") ~ "Prevention",
       str_detect(modality, "VMMC") ~ "Prevention",
       #modality == "VCT" ~ "VCT",
     #  str_detect(modality, "SNSMod") ~ "Community SNS",
       TRUE ~ "Case Finding")
     ) %>%
     group_by(fiscal_year, mod_type, ...) %>%
     summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>%
     ungroup() %>%
     reshape_msd() %>%
     select(-period_type) %>%
     group_by(period, ...) %>%
     mutate(contribution = value/sum(value)) %>%
     ungroup() %>%
     mutate(start = case_when(period == min(period) ~ contribution),
            end = case_when(period == max(period) ~ contribution)) %>%
     mutate(mod_order = fct_reorder(mod_type, value, .desc = T)) %>% 
     complete(mod_type) %>% 
     group_by(mod_type, ...) %>% 
     fill(mod_order, .direction = "up") %>% 
     group_by(period, ...) %>% 
     mutate(pd_tot = sum(value, na.rm = T), 
            pd_25 = pd_tot * 0.25, 
            pd_50 = pd_tot * 0.5,
            pd_75 = pd_tot * 0.75) %>% 
     ungroup() %>% 
     mutate(mod_color = case_when(
       mod_type == "Prevention" ~ scooter, 
       mod_type == "Case Finding" ~ golden_sand,
      # mod_type == "Other PITC" ~ "#AF6458",
       #mod_type == "PMTCT"~ "#736F4C",
       #mod_type == "Community SNS" ~ "#526A83",
       TRUE ~ "#7C7C7C"
     ),
     note = case_when(
       mod_type == "Case Finding" & period == "FY20Q1" ~ "HTS_TST_POS",
       TRUE ~ NA_character_
     )) %>% 
     filter(!is.na(mod_order))
   return(df_hts_full)
 } 

 
 df_modality_age <- munge_modality(df_msd, trendscoarse) %>% 
   left_join(df_undiagnosed, by = c("trendscoarse" = "pop"))
 
#plot by age
 
 
 df_modality_age %>% 
   filter(trendscoarse != "Unknown Age") %>% 
   ggplot(aes(x = period)) +
   geom_col(aes(y = pd_tot), fill = trolley_grey_light) +
   geom_col(aes(y = value, fill = mod_color)) +
   geom_text(data = . %>% filter(trendscoarse == "15+", period == min(period)),
             aes(y = pd_tot, label = paste("Total\nTests")), color = trolley_grey,
             size = 9 / .pt, family = "Source Sans Pro", vjust = 1.2) +
   geom_errorbar(aes(ymin = pd_25, ymax = pd_25), 
                 size = 0.25, color = "white", 
                 linetype = "dotted") +
   geom_errorbar(aes(ymin = pd_50, ymax = pd_50), 
                 size = 0.25, color = "white", 
                 linetype = "dotted") +
   geom_errorbar(aes(ymin = pd_75, ymax = pd_75), 
                 size = 0.25, color = "white", 
                 linetype = "dotted") +
   scale_fill_identity() +
   facet_grid(mod_order ~ fct_reorder(age_facet, value, na.rm = TRUE, .desc = TRUE), scales = "free_y") +
   geom_text(aes(y = value, label = percent(start, 1)), size = 7/.pt, vjust = -0.5, family = "Source Sans Pro") +
   geom_text(aes(y = value, label = percent(end, 1)), size = 7/.pt,  vjust = -0.5, family = "Source Sans Pro") +
   geom_text(data = . %>% filter(trendscoarse == "15+", period == min(period)),
             aes(y = value, label = paste("modality\nshare")),
             size = 6.5/.pt, vjust = 1.5, family = "Source Sans Pro", color = "white") +
   # geom_text(aes(y = pd_tot, label = note), size = 8/.pt, color = "#7C7C7C",
   #           hjust = 0.2, vjust = -0.25) +
   labs(x = NULL, y = NULL,
        title = glue("{cntry %>% toupper} HTS_TST MODALITY SUMMARY BY AGE"),
        subtitle = "Undiagnosed PLHIV proxy calculated as the number of PLHIV minus PLHIV who know their status (UNAIDS 2022 Data)",
        caption = glue("Source: UNAIDS 2022 Epidemiology Data & {metadata$source} | Ref id:{ref_id}")) +
   theme(legend.position = "none") +
   scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
   scale_x_discrete(labels = c("FY20Q1", "", "", "",
                               "FY21Q1", "", "", "",
                               "FY22Q1", "", "", "")) +
   si_style_ygrid(facet_space = 0.5)   
 
 si_save("Images/05_testing_age.png")
 si_save("Graphics/05_testing.svg")
 
 #BY SNU -----------------------------------------
 
 df_modality_snu <- munge_modality(df_msd, snu1)
 
 top_snu <- df_modality_snu %>% 
   filter(period == max(period)) %>% 
   arrange(desc(value)) %>% 
   slice_head() %>% 
   pull(snu1)
 
 df_modality_snu %>% 
   filter(period == max(period)) %>% 
   ggplot(aes(x = period)) +
   geom_col(aes(y = pd_tot), fill = trolley_grey_light) +
   geom_col(aes(y = value, fill = mod_color)) +
   geom_text(data = . %>% filter(snu1 == top_snu, period == min(period)),
             aes(y = pd_tot, label = paste("Total\nTests")), color = trolley_grey,
             size = 9 / .pt, family = "Source Sans Pro", vjust = 1.2) +
   geom_errorbar(aes(ymin = pd_25, ymax = pd_25), 
                 size = 0.25, color = "white", 
                 linetype = "dotted") +
   geom_errorbar(aes(ymin = pd_50, ymax = pd_50), 
                 size = 0.25, color = "white", 
                 linetype = "dotted") +
   geom_errorbar(aes(ymin = pd_75, ymax = pd_75), 
                 size = 0.25, color = "white", 
                 linetype = "dotted") +
   scale_fill_identity() +
   geom_text(data = . %>% filter(snu1 == top_snu, period == min(period)),
             aes(y = value, label = paste("modality\nshare")),
             size = 6.5/.pt, vjust = 1.5, family = "Source Sans Pro", color = "white") +
   facet_grid(mod_order ~ fct_reorder(snu1, value, na.rm = TRUE, .desc = TRUE), scales = "free_y") +
   geom_text(aes(y = value, label = percent(start, 1)), size = 7/.pt, vjust = -0.5) +
   geom_text(aes(y = value, label = percent(end, 1)), size = 7/.pt,  vjust = -0.5) +
   # geom_text(aes(y = pd_tot, label = note), size = 8/.pt, color = "#7C7C7C",
   #           hjust = 0.2, vjust = -0.25) +
   labs(x = NULL, y = NULL,
        title = glue("{cntry %>% toupper} HTS_TST MODALITY SUMMARY BY SNU IN {metadata$curr_pd}"),
        caption = glue("Source: UNAIDS 2022 Epidemiology Data & {metadata$source} | Ref id:{ref_id}")) +
   theme(legend.position = "none") +
   scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
   # scale_x_discrete(labels = c("FY20Q1", "", "", "",
   #                             "FY21Q1", "", "", "",
   #                             "FY22Q1", "", "")) +
   si_style_ygrid(facet_space = 0.5)+
   theme(axis.text.x = element_blank())
 
 si_save("Images/05_testing_snu.png")
 
 