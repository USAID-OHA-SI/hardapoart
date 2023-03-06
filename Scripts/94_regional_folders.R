# PROJECT:  hardapoart
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  export images in regional folders
# REF ID:    
# LICENSE:  MIT
# DATE:     2023-02-24
# UPDATED: 


source(here("Scripts/01_achievement_indicator-psnu.R"))
source(here("Scripts/02_95s_epi.R"))
source(here("Scripts/03_prevalence-gaps_gp-psnu-sex.R"))
source(here("Scripts/04_populationpyramid.R"))
source(here("Scripts/05_testing.R"))
source(here("Scripts/06_linkage_psnu.R"))
source(here("Scripts/07_txcoverage_age_sex.R"))
source(here("Scripts/08_vl-coverage-and-suppression_gp-age.R"))
source(here("Scripts/09_vl-coverage-and-suppression_gp-psnu.R"))
source(here("Scripts/10_prep-trends_age-sex-kp.R"))
source(here("Scripts/11_sid_comp.R"))
source(here("Scripts/12_budget-trends_agency.R"))
source(here("Scripts/13_funding-distro_funding-flavors.R"))
source(here("Scripts/14_hrh-footprint-various.R"))
source(here("Scripts/15_hrh-titles.R"))
source(here("Scripts/16_10s_barriers.R"))
source(here("Scripts/17_vl-coverage-and-suppression_kp-psnu.R"))
source(here("Scripts/18_ovc-coverage_psnu.R"))
source(here("Scripts/19_ovc-budget_agency.R"))
source(here("Scripts/20_dreams_prev-gaps_psnu.R"))
source(here("Scripts/21_tbstat_coverage.R"))
source(here("Scripts/22_tpt_completion.R"))
source(here("Scripts/99_utilities.R"))
source(here("Scripts/98_export_imgs.R"))

regions<-c("Western Hemisphere Region", "West Africa Region","Asia Region")


for (region in regions)
{
vct_cntry <- glamr::pepfar_country_list %>% filter(operatingunit==region) %>%
             select(country)
print(vct_cntry)
walk(vct_cntry$country,~export_imgs(.,agency="PEPFAR"))
}

