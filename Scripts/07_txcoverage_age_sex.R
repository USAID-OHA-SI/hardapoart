# PROJECT:  hardapoart
# AUTHOR:   K. Srikanth | USAID
# REF ID:   725ebd70
# PURPOSE:  coverage gap
# LICENSE:  MIT
# DATE:     2023-02-08
# UPDATED: adapted from groundhog_day/Scripts/FY21Q4_TXCURRSUBNAT-coverage.R

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(gagglr)
library(extrafont)
library(scales)
library(tidytext)
library(patchwork)
library(ggtext)
library(glue)


# GLOBAL VARIABLES --------------------------------------------------------

file_path <- si_path() %>% 
  return_latest("NAT_SUBNAT")

ref_id <- "725ebd70"

get_metadata(file_path)

cntry <- c("Mozambique")

ind_sel <- c("PLHIV", "DIAGNOSED_SUBNAT" ,"TX_CURR_SUBNAT", "VL_SUPPRESSION_SUBNAT")




# IMPORT ------------------------------------------------------------------

df_subnat <- si_path() %>% 
  return_latest("NAT_SUBNAT") %>% 
  read_msd()   

# MUNGE -------------------------------------------------------------------

df_gap <- df_subnat %>% 
  filter(country %in% cntry,
         fiscal_year == 2022,
         indicator %in% ind_sel,
         standardizeddisaggregate == "Age/Sex/HIVStatus") %>% 
  count(country, indicator, ageasentered, sex, wt = targets, name = "value") %>% 
  pivot_wider(names_from = indicator,
              names_glue = "{tolower(indicator)}") %>% 
  mutate(cov_status = diagnosed_subnat/plhiv,
         cov_tx = tx_curr_subnat/plhiv)

df_viz <- df_gap %>% 
  mutate(plhiv_marker = case_when(tx_curr_subnat > plhiv ~ plhiv),
         fill_color = ifelse(sex == "Male", genoa, moody_blue)) %>% 
  group_by(country) %>% 
  mutate(ctry_name = glue("{country}<br>{label_number_si()(sum(tx_curr_subnat, na.rm = TRUE))}/{label_number_si()(sum(plhiv, na.rm = TRUE))}"),
         lab_gap = case_when(cov_tx < .95^2 ~ percent(cov_tx, 1))) %>% 
  ungroup()

# VIZ ---------------------------------------------------------------------


df_viz %>% 
  ggplot(aes(plhiv, ageasentered, fill = fill_color, color = fill_color)) +
  geom_blank(aes(plhiv*1.1)) +
  geom_col(fill = NA, width = .8, alpha = .8) +
  geom_col(aes(tx_curr_subnat), width = .8, alpha = .8) +
  geom_errorbar(aes(xmin = plhiv_marker, xmax = plhiv_marker), 
                na.rm = TRUE, color = "white", linetype = "dotted") +
  geom_text(aes(label = lab_gap), na.rm = TRUE,
            family = "Source Sans Pro", color = suva_grey,
            size = 10/.pt, hjust = -.5) +
  facet_grid(sex ~ fct_reorder(ctry_name, plhiv, sum, na.rm = TRUE, .desc = TRUE),
             switch = "y", scales = "free_x"
  ) +
  scale_x_continuous(labels = label_number_si(),
                     expand = c(.005, .005)) +
  scale_fill_identity(aesthetics = c("fill", "color")) +
  labs(x = NULL, y = NULL,
       title = glue("The largest gaps to treatment coverage in {cntry}") %>% toupper,
       subtitle = "TX_CURR_SUBNAT coverage of PLHIV",
       caption = glue("{metadata$caption}")) +
  coord_cartesian(clip = "off") +
  si_style_xgrid() +
  theme(strip.text.y = element_text(hjust = .5),
        strip.text.x = element_markdown(),
        strip.placement = "outside",
        panel.spacing.x = unit(1, "lines"),
        panel.spacing.y = unit(.5, "lines"))

si_save("Graphics/FY21Q4_TXCURRSUBNAT-coverage.svg")
si_save("Images/FY21Q4_TXCURRSUBNAT-coverage.png")

