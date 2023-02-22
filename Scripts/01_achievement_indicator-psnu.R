# PROJECT:  Hard-A-POART - OU by PSNU
# AUTHOR:   A.Chafetz & N. Petrovic | USAID
# PURPOSE:  Achievement across Indicators & PSNUs
# REF ID:
# LICENSE:  MIT
# DATE:     2022-02-06
# UPDATED:  

prep_achv_psnu <- function (df, cntry, agency){
  
    #clean exit if no data
    if(cntry %ni% unique(df$country) | agency %ni% unique(df$funding_agency))
      return(NULL)
  
    #select indicators
    ind_sel <- c("HTS_TST","HTS_TST_POS", "TX_NEW", "TX_CURR", 
                 "TX_PVLS_D", "TX_PVLS", "PrEP_NEW", "VMMC_CIRC", 
                 "PMTCT_EID", "OVC_SERV", "KP_PREV", "TB_PREV")

    semi_annual <- c("OVC_SERV", "KP_PREV", "TB_PREV")
    
    #function used to clean number format later
    clean_number <- function(x, digits = 0){
                   dplyr::case_when((x >= 1e9 & x < 1e10) ~ glue("{round(x/1e9, digits+1)}B"),
                   (x >= 1e9 & x >= 1e10) ~ glue("{round(x/1e9, digits)}B"),                
                   (x >= 1e6 & x < 1e7) ~ glue("{round(x/1e6, digits+1)}M"),
                   (x >= 1e6 & x >= 1e7) ~ glue("{round(x/1e6, digits)}M"),
                   (x >= 1e3 & x < 1e4) ~ glue("{round(x/1e3, digits+1)}K"),
                   (x >= 1e3 & x >= 1e4) ~ glue("{round(x/1e3, digits)}K"),
                   TRUE ~ glue("{x}"))
                    }

    #subset to key indicators
    df_achv <- df %>% 
               filter(country==cntry,
               funding_agency == agency,
               indicator %in% ind_sel,
               fiscal_year==metadata_msd$curr_fy) 

    if(nrow(df_achv) == 0)
      return(NULL)
    
    ## Aggregating results & targets at the OU level for each indicator
    df_achv <- df_achv %>% 
               bind_rows(df_achv %>% mutate(psnuuid = "GLOBAL")) %>% 
               pluck_totals() %>% 
               group_by(fiscal_year, country, funding_agency, psnuuid, indicator) %>%
               summarize(across(c(targets, cumulative), \(x) sum(x, na.rm = TRUE)), 
               .groups = "drop")

    #remove data points at Q1 for semi-annual indicators
    if(metadata_msd$curr_qtr == 1){
      df_achv <- df_achv %>%
        dplyr::mutate(cumulative = ifelse(indicator %in% semi_annual, 
                                          NA_real_, cumulative))
    }
    
   #calculate achievement and add colors 
   df_achv <- adorn_achievement(df_achv, metadata_msd$curr_qtr)
   

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
         mutate(indicator=case_when(indicator %in% snapshot_ind ~ paste(indicator, "(SS)"), TRUE ~ indicator)) %>%
         mutate(ind_w_glob_vals = case_when(psnuuid == "GLOBAL" & is.na(targets) ~ glue("**{indicator}**<br><span style = 'font-size:11pt;'>No MER reporting</span>"),
                                  psnuuid == "GLOBAL" ~ glue("**{indicator}**<br><span style = 'font-size:11pt;'>{clean_number(cumulative)} / {clean_number(targets)}</span>"))) %>% 
         group_by(indicator) %>% 
         fill(ind_w_glob_vals, .direction = "downup") %>% 
         ungroup() %>% 
         arrange(indicator) %>% 
         mutate(ind_w_glob_vals = fct_inorder(ind_w_glob_vals)) 
     
  
  
    return(df_achv_viz)
  
   }

# VIZ - ACHIEVEMENT BY COUNTRY -------------------------------------------------------

viz_achv_psnu <- function (df){

  if(is.null(df) || nrow(df) == 0)
    return(print(paste("No data available.")))
    
    #Reference ID to be used for searching GitHub
    ref_id <- "d51dd3f9"
    vrsn <- 1 
    
    lab_q4<-c("<75%","75-89%","90-110%","+110%")
    
    lab_leg<-case_when(metadata_msd$curr_qtr==1 ~  c("<15%","15-35%",">35%"))%>%
    ## NOTE: Will need to add Q2, Q3, Q4 later
                      # metadata_msd$curr_qtr==2 ~  c("<75%","75-89%","90-110%","+110%"),
                      # metadata_msd$curr_qtr==3 ~  c("<75%","75-89%","90-110%","+110%"),
                      # metadata_msd$curr_qtr==4 ~  c("<75%","75-89%","90-110%","+110%")) %>%
              paste("| SS:",lab_q4)

    
    df %>% 
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
        scale_color_identity(guide="legend", breaks=c("#ff939a","#ffcaa2","#5BB5D5","#e6e6e6"),
                             labels=lab_leg,
                             name="Achievement: Cumulative indicators | Snapshot indicators") + #whatever value is defined by color -- use that value from data frame
        facet_wrap(~ind_w_glob_vals, scales = "free_y", nrow=2) +
        labs(x = NULL, y = NULL,
             title = glue("{metadata_msd$curr_pd} {unique(df$funding_agency)}/{unique(df$country)} achievement, year to date") %>% toupper,
             subtitle = glue("Country achievement (large, labeled points) with PSNU achievement reference points, SS indicates snapshot indicator<br>"),
             caption = glue("Target achievement capped at 110%
                              Source: {metadata_msd$source} | USAID/OHA/SIEI |  Ref ID: {ref_id} v{vrsn}")) +
        si_style_nolines() +
        theme(
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          plot.subtitle = element_markdown(),
          strip.text = element_markdown(),
          panel.spacing.y = unit(0, "lines"),
          legend.position="bottom")
          #legend.title=element_text(vjust = 2, hjust=0.5))
       
}  
  





