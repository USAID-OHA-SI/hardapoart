library(gagglr)
library(tidyverse)
library(scales)
library(sf)
library(extrafont)
library(tidytext)
library(patchwork)
library(ggtext)
library(glue)
library(countrycode)
library(mindthegap)

# SI specific paths/functions  
load_secrets()

# Setting relative paths to data using SI infrastructure
merdata <- file.path(glamr::si_path("path_msd"))
file_path <- return_latest(folderpath = merdata, pattern = "OU_IM_FY20-23")

# Grab metadata
get_metadata(file_path)

# Load the MSD from Panorama, this is where the topline #s are from
msd <- read_msd(file_path)

# Reference ID to be used for searching GitHub
ref_id <- "0530547f"

# Select country for image
country_sel<-"Tanzania"

## Filter for age/sex and KP disaggregates. Trend starts at FY22Q1
df <- msd %>% 
  filter(indicator == "PrEP_NEW", 
         standardizeddisaggregate %in% c("Age/Sex", "KeyPopAbr"),
         country==country_sel,
         funding_agency=="USAID",
         fiscal_year>2021) 

df_prep <- df %>% 
  # Combine age groups above 35+, rename People in Prisons
  mutate(ageasentered = case_when(ageasentered %in% c("35-39","40-44","45-49","50+") ~ "35+", 
                                  TRUE ~ ageasentered),
         otherdisaggregate = case_when(otherdisaggregate %in% "People in prisons and other enclosed settings" ~ "Prisoners", 
                                       TRUE ~ otherdisaggregate)) %>%
  # Create index for faceting - either age or KP group
  mutate(facet_ind=case_when(standardizeddisaggregate == "Age/Sex" ~ ageasentered, TRUE ~ otherdisaggregate)) %>%
  filter(facet_ind!="Unknown Age") %>%
  # Fill sex indicator with "KP" value for KP - will be used for color
  mutate(sex = case_when(is.na(sex)==TRUE  ~ "KP", TRUE ~ sex)) %>%
  group_by(fiscal_year, standardizeddisaggregate, sex, ageasentered, otherdisaggregate, facet_ind) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  reshape_msd() %>% 
  select(-period_type) %>% 
  arrange(period) 


# Facets
  ggplot(df_prep, aes(x=period, y=value)) + 
  geom_point(aes(color=sex), size = 4) +  
  geom_line(aes(group=sex, color=sex), linewidth=1)+
  scale_color_manual(values = c("Female" = moody_blue, "Male" = genoa, "KP"=burnt_sienna),
                     labels = function(x) str_to_upper(x)) +
  facet_wrap(facet_ind ~ ., scales = "fixed", nrow=2, dir="h") + 
  si_style_yline() +
  theme(strip.text.x = element_text(size = 14))+
  theme(legend.position = "none") + 
  coord_cartesian(clip="off")+
  labs(x = NULL, y = NULL,
       title = glue("PREP_NEW DISAGGREGATED BY AGE/SEX (<span style = 'color: #8980cb;'>FEMALE</span><span style = 'color: #287c6f;'>/MALE</span>) 
       AND <span style = 'color: #e07653;'>KEY POPULATIONS</span> IN {toupper(country_sel)}"),  
       caption = glue("Source: {metadata$source} 
                       SI analytics US Agency for International Development | Ref ID: {ref_id}"))+
  theme(plot.title = element_markdown())
 

si_save("Images/PrEP_age_sex_KP_combo.png", width=14, height=7)

