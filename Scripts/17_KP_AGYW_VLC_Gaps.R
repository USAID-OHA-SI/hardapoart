# PROJECT:  hardapoart
# AUTHOR:   Bourke Betz | USAID
# PURPOSE:  Visualization of KP and AGYW Coverage gaps by orgunit
# REF ID:   
# LICENSE:  MIT
# DATE:     2023-02-13
# UPDATED: 
# NOTE:     based on KP Dashboard


cntry <- "Malawi"
# MUNGE -------------------------------------------------------------------

  vl_indicators <- c("TX_CURR", "TX_PVLS", "TX_PVLS_D")
  young <- c("15-19", "20-24", "24-29")
  older <- c("30-34", "35-39", "40-44")

  #filter to select indicators + country
  df_vl <- df_msd %>%  
    filter(indicator %in% vl_indicators,
           country == cntry)
  

  #clean define groups - Total, KP, AGYW, Non-AGYW
  df_vl <- df_vl %>% 
    mutate(type = case_when(sex=="Female" & ageasentered %in% young ~ "AGYW",
                              str_detect(standardizeddisaggregate, "Total|KeyPop", negate = TRUE) ~ "Non-AGYW",
                              TRUE ~ str_extract(standardizeddisaggregate, "Total|KeyPop")))
  
  #aggregate & reshape long
  df_vl <- df_vl %>% 
    group_by(fiscal_year, country, psnu, indicator, type) %>% 
    summarise(across(starts_with("qtr"), \(x) sum(x, na.rm = TRUE)),
              .groups = "drop") %>% 
    reshape_msd(include_type = FALSE)
  
  #pivot wide to subtract KP from GP (Total)
  df_vl <- df_vl %>% 
    pivot_wider(names_from = type,
                values_fill = 0) %>% 
    mutate(GenPop = Total - KeyPop) %>% 
    select(-Total) %>% 
    pivot_longer(-where(is.character),
                 names_to = "type") %>% 
    mutate(group = case_when(type %in% c("KeyPop", "GenPop") ~ "KP-GP",
                             type %in% c("AGYW", "Non-AGYW") ~ "AGYW"),
           .before = type)
  
  #reshape wider by indicator and create lag for VLC
  df_vl <- df_vl %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}") %>% 
    group_by(country, psnu, group, type) %>% 
    mutate(tx_curr_lag2 = lag(tx_curr, n = 2, order_by = period)) %>% 
    ungroup()


  #create populations for comparision
  df_vl %>% 
    mutate(pop = case_when(agesex == "AGYW" ~ agesex,
                           disagg == "Age/Sex" ~ "non-AGYW",
                           disagg == "KP-GP" ~ kpgp),
           disagg = ifelse(disagg == "KP", "KP-GP", disagg)) %>% 
    distinct(agesex, disagg, pop)
  
#separate and bind dataframes

### KP
kp <- vl_comps %>% filter(disagg == "KP") %>% 
  mutate(disagg = "KP-GP") %>% 
  rename(pop = kpgp) %>%
  select(-agesex) %>%
  glimpse()


### GP
gp_pre <- vl_comps %>% filter(disagg == "Total") %>%
  mutate(disagg = NA) %>%
  glimpse()

kp_minus <- kp %>% 
  mutate(
    disagg = NA,
    kpgp = NA,
    TX_PVLS_D = -TX_PVLS_D,
    TX_PVLS_N = -TX_PVLS_N,
    TX_CURR_Lag2 = -TX_CURR_Lag2
  ) %>%
  glimpse()

gp <- bind_rows(gp_pre, kp_minus) %>%
  mutate(pop = "GP",
         disagg = "KP-GP") %>%
  select(-agesex, -kpgp) %>%
  group_by(across(-c("TX_PVLS_D", "TX_PVLS_N", "TX_CURR_Lag2"))) %>%
  summarise(TX_PVLS_D = coalesce(sum(TX_PVLS_D), 0),
            TX_PVLS_N = coalesce(sum(TX_PVLS_N), 0),
            TX_CURR_Lag2 = coalesce(sum(TX_CURR_Lag2),0),
            .groups = "drop") %>%
  glimpse()


# combine dataframes AGYW - vs non-AGYW, KP vs GP 
vl_comparisons <- bind_rows(agyw, nonagyw, kp, gp) %>% 
  # select(-) %>%
  group_by(across(-c("TX_PVLS_D", "TX_PVLS_N", "TX_CURR_Lag2"))) %>%
  summarise(TX_PVLS_D = coalesce(sum(TX_PVLS_D), 0),
            TX_PVLS_N = coalesce(sum(TX_PVLS_N), 0),
            TX_CURR_Lag2 = coalesce(sum(TX_CURR_Lag2),0),
            .groups = "drop") %>%
  clean_names() %>%
  mutate(
    # country = fct_reorder(country, desc(country)),
    vlc = tx_pvls_d/tx_curr_lag2,
    vlc = if_else(vlc > 1, 1.05, vlc),
    vls = tx_pvls_n/tx_pvls_d,
    vls = if_else(vls > 1, 1.05, vls),
  ) %>%
  glimpse()


# visualize after inputting selections ------------------------------------
country_selection <- c("Botswana")
disagg_selection <- c(
   "KP-GP"
  # "Age/Sex"
)

info <- vl_comparisons %>% filter(disagg %in% disagg_selection,
                                  country %in% country_selection) %>% 
  select(operatingunit, country, fy, psnu, disagg) %>% group_by_all() %>% summarise(.groups = "drop") %>%
  mutate(fyy = fy-2000,
         main = if_else(disagg == "KP-GP", "KP", "AGYW"),
         ref = if_else(disagg == "KP-GP", "GP", "non-AGYW"))

vl_comparisons %>% filter(disagg %in% disagg_selection,
                          country %in% country_selection) %>%
  mutate(main = if_else(disagg == "KP-GP", "KP", "AGYW"),
         vlc_main = case_when(pop == main ~ vlc)) %>% 
  arrange(desc(vlc_main)) %>%
  mutate(psnu = fct_reorder(psnu, desc(vlc_main)))  %>%
  #unable to reorder? any assistance is appreciated
  
  ggplot(aes(y = psnu, x = vlc, color = pop)) + 
  geom_line(color = "#d3d3d3") +
  geom_point(size = 2) +
  # geom_text(aes(label=scales::percent(vlc, accuracy = 1))) + 
  scale_color_manual(values = c(KP = scooter, GP = grey30k, AGYW = genoa, "non-AGYW" = grey30k)) +
  si_style_xyline() +
  scale_x_continuous(labels = scales::percent, name = NULL, limits = c(0,1.1), breaks = seq(0,1.1, by = .25)) + 
  theme(legend.title = element_blank(), legend.position = "bottom") +
  labs(title = glue("FY{info$fyy} PEPFAR VL Coverage comparison of {info$main} and {info$ref} by psnu"),
       caption = "VL Coverage > 1, coded as 105%.
       Data Source: MER_Structured_Datasets_OU_IM_FY20-23_20221216_v2_1")

ggsave("Images/vl_comparison_by{info$disagg}.png", width = 13, height = 6)
