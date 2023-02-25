# PROJECT:  hardapoart
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  export images
# REF ID:    
# LICENSE:  MIT
# DATE:     2023-02-24
# UPDATED: 

export_imgs <- function(cntry, agency, folder_out = "Images"){
  
  cntry_info <- pepfar_country_list %>% 
    filter(country == cntry) %>% 
    mutate(clean = ifelse(operatingunit != country, 
                          glue("{operatingunit}/{country}"), 
                          country) %>% str_remove_all("( |\')")) 
  
  cntry_clean <- cntry_info$clean
  cntry_iso <- tolower(cntry_info$country_iso)
  agency_lwr <- tolower(agency)
  
  fldr <- file.path(folder_out, cntry_clean)
  
  if(fs::dir_exists(fldr))
    fs::dir_delete(fldr)
  
  fs::dir_create(fldr)
  
  
  #achievement
  df_msd %>%
    prep_achv_psnu(cntry,agency) %>%
    viz_achv_psnu()
  
  si_save(file.path(fldr, glue("{length(list.files(fldr, 'png')) + 1}_{cntry_iso}_{agency_lwr}_achv")), device = "png")
  
  #epi-95s
  viz_unaids_all(cntry)
  
  si_save(file.path(fldr, glue("{length(list.files(fldr, 'png')) + 1}_{cntry_iso}_epi-95s")), device = "png")
  
  # prevalence
  df_natsubnat %>% 
    prep_hiv_prevalence(cntry) %>% 
    viz_hiv_prevalence()
  
  si_save(file.path(fldr, glue("{length(list.files(fldr, 'png')) + 1}_{cntry_iso}_pepfar_prev-gp")), device = "png")
  
  # prevalence_dreams
  df_natsubnat %>% 
    prep_hiv_prev_DREAMS(cntry) %>% 
    viz_hiv_prev_DREAMS()
  
  si_save(file.path(fldr, glue("{length(list.files(fldr, 'png')) + 1}_{cntry_iso}_pepfar_prev-dreams")), device = "png")
  
  # youth buldge
  df_natsubnat %>% 
    prep_pop_pyramid(cntry, "country") %>% 
    viz_pop_pyramid()
  
  si_save(file.path(fldr, glue("{length(list.files(fldr, 'png')) + 1}_{cntry_iso}_youthbulge")), device = "png")
  
  #youth_bulge_dreams
  df_natsubnat %>% 
    prep_pop_pyramid(cntry, "psnu") %>% 
    prep_pop_pyramid_dreams(df_msd) %>% 
    viz_pop_pyramid(type = "DREAMS")
  
  si_save(file.path(fldr, glue("{length(list.files(fldr, 'png')) + 1}_{cntry_iso}_pepfar_youthbulge-dreams")), device = "png")
  
  # ovc_tx
  prep_ovc_coverage(df_msd, df_subnat, cntry, "PEPFAR") %>% 
    viz_ovc_coverage()
  
  si_save(file.path(fldr, glue("{length(list.files(fldr, 'png')) + 1}_{cntry_iso}_pepfar_ovctx")), device = "png")
  
  #testing
  prep_modality_age(df_msd, cntry, "PEPFAR", trendscoarse) %>% 
    viz_modality_age()
  
  si_save(file.path(fldr, glue("{length(list.files(fldr, 'png')) + 1}_{cntry_iso}_pepfar_testing")), device = "png")
  
  #tb_stat_coverage
  prep_tbstat_cov(df_msd, cntry, agency) %>% 
    viz_tbstat_cov()
  
  si_save(file.path(fldr, glue("{length(list.files(fldr, 'png')) + 1}_{cntry_iso}_{agency_lwr}_tbstat-cov")), device = "png")
  
  #linkage
  df_msd %>% 
    prep_linkage_psnu(cntry, agency) %>% 
    viz_linkage_psnu()
  
  si_save(file.path(fldr, glue("{length(list.files(fldr, 'png')) + 1}_{cntry_iso}_{agency_lwr}_linkage")), device = "png")
  
  #tx_gap
  viz_tx_all(cntry, agency) 
  
  si_save(file.path(fldr, glue("{length(list.files(fldr, 'png')) + 1}_{cntry_iso}_{agency_lwr}_txgap")), device = "png")
  
  #vlsc_pop
  df_msd %>% 
    prep_viral_load(cntry, agency) %>% 
    viz_viral_load()
  
  si_save(file.path(fldr, glue("{length(list.files(fldr, 'png')) + 1}_{cntry_iso}_{agency_lwr}_vl-gp")), device = "png")
  
  #vlsc_kp
  df_msd %>% 
    prep_viral_load_kp_agyw(cntry, agency) %>% 
    viz_viral_load_kp_agyw()
  
  si_save(file.path(fldr, glue("{length(list.files(fldr, 'png')) + 1}_{cntry_iso}_{agency_lwr}_vl-kp")), device = "png")
  
  #vlsc_geo
  df_msd %>% 
    prep_viral_load_psnu(cntry, agency) %>% 
    viz_viral_load_psnu()
  
  si_save(file.path(fldr, glue("{length(list.files(fldr, 'png')) + 1}{cntry_iso}_{agency_lwr}_vl-psnu}")), device = "png")
  
  
  #prep
  df_msd %>%
    prep_prep_disagg(cntry, agency) %>%
    viz_prep_disagg()
  
  si_save(file.path(fldr, glue("{length(list.files(fldr, 'png')) + 1}_{cntry_iso}_{agency_lwr}_prep")), device = "png")
  
  #tpt
  df_msd %>%
    prep_tpt(params$cntry, params$agency) %>%
    viz_tpt()
  
  si_save(file.path(fldr, glue("{length(list.files(fldr, 'png')) + 1}_{cntry_iso}_{agency_lwr}_tpt")), device = "png")
  
  
  #sid
  df_sid %>% 
    prep_sid(cntry) %>% 
    viz_sid()
  
  si_save(file.path(fldr, glue("{length(list.files(fldr, 'png')) + 1}_{cntry_iso}_sid")), device = "png")
  
  # budget_share
  df_fsd %>% 
    prep_budget_trends(cntry) %>% 
    viz_budget_trends()
  
  si_save(file.path(fldr, glue("{length(list.files(fldr, 'png')) + 1}_{cntry_iso}_budget-share")), device = "png")
  
  #ovc_budget}
  df_msd %>% 
    prep_ovc_budget(df_fsd, cntry) %>% 
    viz_ovc_budget()
  
  si_save(file.path(fldr, glue("{length(list.files(fldr, 'png')) + 1}_{cntry_iso}_budget-ovc")), device = "png")
  
  #funding_distro
  df_fsd %>% 
    prep_funding_distro(cntry, "USAID") %>% 
    viz_funding_distro()
  
  si_save(file.path(fldr, glue("{length(list.files(fldr, 'png')) + 1}_{cntry_iso}_usaid_funding-distro")), device = "png")
  
  #staffing_footprint
  df_hrh %>% 
    prep_hrh_footprint(cntry, "USAID") %>% 
    viz_hrh_footprint()
  
  si_save(file.path(fldr, glue("{length(list.files(fldr, 'png')) + 1}_{cntry_iso}_usaid_staff-footprint")), device = "png")

  #staffing_titles
  df_hrh %>% 
    prep_hrh_titles(cntry, "USAID") %>% 
    viz_hrh_titles()
  
  si_save(file.path(fldr, glue("{length(list.files(fldr, 'png')) + 1}_{cntry_iso}_usiad_staff-titles_usaid")), device = "png")
  
  #10s_barriers
  df_tens %>% 
    prep_10s_barriers(cntry) %>% 
    viz_10s_barriers()
  
  si_save(file.path(fldr, glue("{length(list.files(fldr, 'png')) + 1}_{cntry_iso}_10s-barriers")), device = "png")
  
}
