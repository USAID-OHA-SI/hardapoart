# PROJECT:  hardapoart
# AUTHOR:   N.Petrovic | USAID
# PURPOSE:  PrEP_NEW broken down by Age/Sex & KP
# REF ID:   0530547f
# LICENSE:  MIT
# DATE:     2023-02-02
# UPDATED:  

prep_prep_disagg <- function (df, cntry, agency){

      ## Filter for age/sex and KP disaggregates. Trend starts at FY22Q1
      df_prep<-df %>% 
      filter(indicator == "PrEP_NEW", 
             standardizeddisaggregate %in% c("Age/Sex", "KeyPopAbr"),
             country==cntry,
             funding_agency==agency) %>% 
      # Combine age groups above 35+, rename People in Prisons
      mutate(ageasentered = case_when(ageasentered %in% c("35-39","40-44","45-49","50+") ~ "35+", 
                            TRUE ~ ageasentered),
             otherdisaggregate = case_when(otherdisaggregate %in% "People in prisons and other enclosed settings" ~ "Prisoners", 
                            TRUE ~ otherdisaggregate)) %>%
      # Create index for faceting - either age or KP group
      mutate(facet_ind=case_when(standardizeddisaggregate == "Age/Sex" ~ ageasentered, 
                            TRUE ~ otherdisaggregate)) %>%
      filter(facet_ind!="Unknown Age") %>%
      # Fill sex indicator with "KP" value for KP - will be used for color
      mutate(sex = case_when(is.na(sex)==TRUE  ~ "KP", TRUE ~ sex)) %>%
      group_by(fiscal_year, standardizeddisaggregate, sex, ageasentered, otherdisaggregate, facet_ind) %>% 
      summarise(across(starts_with("qtr"), \(x) sum(x, na.rm = TRUE))) %>% 
      ungroup() %>% 
      reshape_msd() %>% 
      select(-period_type) %>% 
      arrange(period) 

}

viz_prep_disagg <-function (df, cntry, agency){
  
  # Reference ID to be used for searching GitHub
  ref_id <- "0530547f"

  # Facets
  ggplot(df, aes(x=period, y=value)) + 
  geom_point(aes(color=sex), size = 4) +  
  geom_line(aes(group=sex, color=sex), linewidth=1)+
  scale_color_manual(values = c("Female" = moody_blue, "Male" = genoa, "KP"=burnt_sienna),
                     labels = function(x) str_to_upper(x)) +
  facet_wrap(facet_ind ~ ., scales = "fixed", ncol=5, dir="h") + 
  si_style_yline() +
  theme(strip.text.x = element_text(size = 14, face="bold"),
        axis.text.x = element_text(size=11, angle=45),
        axis.text.y = element_text(size=11, angle=0))+
  theme(legend.position = "none") + 
  coord_cartesian(clip="off")+
  labs(x = NULL, y = NULL,
       title = glue("PREP_NEW DISAGGREGATED BY AGE/SEX (<span style = 'color: #8980cb;'>FEMALE</span><span style = 'color: #287c6f;'>/MALE</span>) 
       AND <span style = 'color: #e07653;'>KEY POPULATIONS</span> IN {toupper(cntry)}, {toupper(agency)} <br />"),  
       caption = glue("Source: {metadata_msd$source} 
                       SI analytics US Agency for International Development | Ref ID: {ref_id}"))+
  theme(plot.title = element_markdown())
} 



