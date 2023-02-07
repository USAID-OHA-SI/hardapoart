# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  10-10-10 progress
# REF ID:   1e748b9a 
# LICENSE:  MIT
# DATE:     2023-02-07
# UPDATED: 
# SOURCE:   https://hivpolicylab.org/data
# NOTE: adapted from catch-22/Scripts/2021115_Global Planning Meeting/gpm_pepfar_10s-gap.R

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(gagglr)
library(extrafont)
library(scales)
library(tidytext)
library(patchwork)
library(ggtext)
library(glue)
library(googlesheets4)
library(janitor)


# GLOBAL VARIABLES --------------------------------------------------------

load_secrets()

gs_id <- as_sheets_id("1LadXX9g9D6MCp6M3WD27Js85YE8MHrf_ulJQmRVduCU")

ind_sel <- c(paste0("S", 1:6), "S9")

cntry <- "Mozambique"

ref_id <- "1e748b9a"


# IMPORT ------------------------------------------------------------------

#read in HIV Policy Lab data export
df <- read_sheet(gs_id, "Policy adoption data", skip = 6,
                 col_types = "c",
                 .name_repair = make_clean_names)


#limit to just relevant indicators & rename
df_struct <- df %>% 
  rename(indicator = indicator_subindicator_name) %>% 
  filter(indicator %in% ind_sel) %>% 
  mutate(indicator_name = recode(indicator,
                                 "S1" = "Same-sex sex non-criminalization",
                                 "S2" = "Sex work non-criminalization",
                                 "S3" = "Drug use non-criminalization",
                                 "S4" = "HIV exposure non-criminalization",
                                 "S5" = "Non-discrimination protections",
                                 "S6" = "National human rights institutions",
                                 "S9" = "Gender based violence"))

#rename countries and limit to just PEPFAR
df_struct <- df_struct %>% 
  mutate(country = ifelse(str_detect(country, "Ivoire"), "Cote d'Ivoire", country),
         country = recode(country,
                          "Myanmar" = "Burma",
                          "Lao People's Democratic Republic" = "Laos",
                          "Tanzania (United Republic of)" = "Tanzania",
                          "Viet Nam" = "Vietnam"
         )) %>% 
  filter(country %in% pepfar_country_list$country)

#aggregate adoption across PEPFAR countries
df_viz <- df_struct %>% 
  filter(year == "Most recent",
         country == cntry) %>% 
  count(adoption_level, indicator_name) %>% 
  filter(!is.na(adoption_level))

df_viz <- df_viz %>% 
  mutate(adoption_level = factor(adoption_level, c("Adopted", "Partial", "Not adopted")),
         indicator_order = case_when(adoption_level == "Not adopted" ~ 3,
                                     adoption_level == "Partial" ~ 2, 
                                     TRUE ~ 1),
         fill_color = case_when(adoption_level == "Not adopted" ~ old_rose,
                                adoption_level == "Partial" ~ burnt_sienna_light,
                                adoption_level == "Adopted" ~ scooter_med))


df_viz %>% 
  ggplot(aes(n, fct_reorder(indicator_name, indicator_order, na.rm = TRUE))) +
  geom_col(aes(fill = fill_color)) +
  geom_vline(xintercept = 0) +
  facet_wrap(~fct_rev(adoption_level)) +
  scale_fill_identity() +
  scale_x_continuous(position = "top") +
  labs(x = NULL, y = NULL,
       title = glue("THE LARGEST GAP IN THE 10-10-10 GOALS IN {cntry %>% toupper()}"),
       subtitle = "Progress towards adopting structural laws/policies towards UNAIDS' 10-10-10 goals",
       caption = glue("Source: HIV Policy Lab [2021-11-09]",
                      "{ref_id}", .sep = " | ")) +
  si_style_nolines() +
  theme(strip.placement = "outside",
        axis.text.x = element_blank())

si_save("Images/15_10s_barriers.png")
si_save("Graphics/15_10s_barriers.svg")


si_save("Graphics/gpm_pepfar_10s-gap.svg", height = 4.25)  
