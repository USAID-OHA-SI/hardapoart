# PROJECT:  Hard-A-POART - OU by PSNU
# AUTHOR:   A.Chafetz & N. Petrovic | USAID
# PURPOSE:  
# LICENSE:  MIT
# DATE:     2022-02-06


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(glitr)
library(glamr)
library(gophr)
library(extrafont)
library(scales)
library(tidytext)
library(patchwork)
library(ggtext)
library(glue)
library(gisr)
library(sf)
library(ggrepel)


# GLOBAL VARIABLES --------------------------------------------------------

load_secrets()
merdata <- file.path(glamr::si_path("path_msd"))
file_path <- return_latest(folderpath = merdata,
                           pattern = "PSNU_IM_FY20-23")

#select indicators
ind_sel <- c("HTS_TST","HTS_TST_POS", "TX_NEW", "TX_CURR", "TX_PVLS_D", "TX_PVLS", 
             "PrEP_NEW", "VMMC_CIRC", "OVC_SERV", "KP_PREV", "PMTCT_EID", "TB_PREV")

#caption info for plotting
source <- source_info(file_path)

# Reference ID to be used for searching GitHub
ref_id <- "d51dd3f9"


#current FY and quarter
get_metadata(file_path)
curr_fy <- source_info(file_path, return = "fiscal_year")
curr_qtr <- source_info(file_path, return = "quarter")

clean_number <- function(x, digits = 0){
  dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                   x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                   x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                   TRUE ~ glue("{x}"))
}


# IMPORT ------------------------------------------------------------------

df <- read_msd(file_path)   


# MUNGE -------------------------------------------------------------------
country_select<-"Colombia"

#subset to key indicators
df_achv <- df %>% 
  clean_indicator() %>%
  filter(country==country_select,
         funding_agency == "USAID",
         fiscal_year == curr_fy,
         indicator %in% ind_sel) 


# MUNGE - GLOBAL/COUNTRY ACHIEVEMENT ---------------------------------------------

## Aggregating results & targets at the OU level for each indicator
df_achv <- df_achv %>% 
  bind_rows(df_achv %>% 
              mutate(psnuuid = "GLOBAL")) %>% 
  filter(standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>% 
  group_by(fiscal_year, country, psnuuid, indicator) %>% 
  summarize(across(c(targets, cumulative), sum, na.rm = TRUE), 
            .groups = "drop")

#calculate achievement and add colors 
df_achv <- df_achv %>% 
  adorn_achievement(metadata$curr_qtr)

#viz adjustments
df_achv_viz <- df_achv %>% 
  mutate(global_achv = case_when(psnuuid == "GLOBAL" ~ achievement),
         achievement = ifelse(psnuuid == "GLOBAL", NA, achievement),
         indicator = factor(indicator, ind_sel),
         baseline_pt_1 = 0,
         baseline_pt_2 = .25,
         baseline_pt_3 = .5,
         baseline_pt_4 = .75,
         baseline_pt_5 = 1,
  )

#adjust facet label to include indicator and national values
df_achv_viz <- df_achv_viz %>% 
  mutate(ind_w_glob_vals = case_when(psnuuid == "GLOBAL" & is.na(targets) ~ 
                                       glue("**{indicator}**<br><span style = 'font-size:11pt;'>No MER reporting</span>"),
                                     psnuuid == "GLOBAL" ~ 
                                       glue("**{indicator}**<br><span style = 'font-size:11pt;'>{clean_number(cumulative)} / 
                                            {clean_number(targets)}</span>"))) %>% 
  group_by(indicator) %>% 
  mutate(rank_worst=rank(achievement, ties.method="min")) %>%
  fill(ind_w_glob_vals, .direction = "downup") %>% 
  ungroup() %>% 
  arrange(indicator) %>% 
  mutate(ind_w_glob_vals = fct_inorder(ind_w_glob_vals))


# VIZ - ACHIEVEMENT BY COUNTRY -------------------------------------------------------
df_achv_viz %>% 
  ggplot(aes(achievement, indicator, color = achv_color)) +
  geom_blank() + # creates blank canvas +
  geom_linerange(aes(xmin = 0, xmax = 1.1, y = 1), color = "#D3D3D3") +
  geom_point(aes(x=baseline_pt_1), shape = 3, color = "#D3D3D3") +
  geom_point(aes(x=baseline_pt_2), shape = 3, color = "#D3D3D3") +
  geom_point(aes(x=baseline_pt_3), shape = 3, color = "#D3D3D3") +
  geom_point(aes(x=baseline_pt_4), shape = 3, color = "#D3D3D3") +
  geom_point(aes(x=baseline_pt_5), shape = 3, color = "#D3D3D3") +
  geom_jitter(position = position_jitter(width = 0, height = 0.1), na.rm = TRUE,
              alpha = .7, size = 3) + 
  geom_point(aes(global_achv), size = 10, alpha = 1, na.rm = TRUE, 
             position=position_nudge(y=0.3)) +
  geom_text(aes(global_achv, label = percent(global_achv, 1)), na.rm = TRUE,
            position=position_nudge(y=0.3),
            color = "#202020", family = "Source Sans Pro", size = 10/.pt) +
  coord_cartesian(clip = "off") + # default decides how much to show - expands padding
  scale_x_continuous(limit=c(0,1.1),oob=scales::squish, breaks = seq(0, 1.25, .25), label = percent_format(1)) + #capping achievement at 110
  scale_color_identity() + #whatever value is defined by color -- use that value from data frame
  facet_wrap(~ind_w_glob_vals, scales = "free_y", nrow=2) +
  labs(x = NULL, y = NULL,
       title = glue("{metadata$curr_pd} {country_select} achievement, year to date, USAID ") %>% toupper,
       subtitle = glue("Country achievement (large, labeled points) with PSNU achievement reference points <br>"),
       caption = glue("Target achievement capped at 110%
                        Source: {metadata$source}, US Agency for International Development | Ref ID: {ref_id}")) +
  si_style_nolines() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    plot.subtitle = element_markdown(),
    strip.text = element_markdown(),
    panel.spacing.y = unit(0, "lines"))

#si_save(glue("Graphics/FY{metadata$curr_pd}_achv_psnu.svg"))
si_save(glue("Images/FY{curr_fy}Q{curr_qtr}_achv_psnu.png"))



