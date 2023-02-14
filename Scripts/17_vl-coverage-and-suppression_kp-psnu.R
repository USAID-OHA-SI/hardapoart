# PROJECT:  hardapoart
# AUTHOR:   Bourke Betz | USAID
# PURPOSE:  Visualization of KP and AGYW Coverage gaps by orgunit
# REF ID:   
# LICENSE:  MIT
# DATE:     2023-02-13
# UPDATED:  2023-02-14
# NOTE:     based on KP Dashboard


# MUNGE -------------------------------------------------------------------

prep_viral_load_kp_agyw <- function(df, cntry, agency){
  
  young <- c("10-14","15-19", "20-24") #DREAMS AGYW age band based on DREAMS guidance
  
  #filter to select indicators + country
  df_vl <- df_msd %>%  
    filter(indicator %in% c("TX_CURR", "TX_PVLS", "TX_PVLS_D"),
           country == cntry,
           funding_agency == agency)
  
  #clean exit for missing data
  if(nrow(df_vl) == 0)
    return(NULL)
  
  #create overall value
  df_vl <- df_vl %>% 
    bind_rows(df_vl %>% 
                mutate(psnu = "OVERALL"))
  
  #clean define groups - Total, KP, AGYW, Non-AGYW
  df_vl <- df_vl %>% 
    mutate(type = case_when(sex=="Female" & ageasentered %in% young ~ "AGYW",
                            str_detect(standardizeddisaggregate, "Total|KeyPop", negate = TRUE) ~ "Non-AGYW",
                            TRUE ~ str_extract(standardizeddisaggregate, "Total|KeyPop")))
  
  #aggregate & reshape long
  df_vl <- df_vl %>% 
    group_by(fiscal_year, funding_agency, country, psnu, indicator, type) %>% 
    summarise(across(starts_with("qtr"), \(x) sum(x, na.rm = TRUE)),
              .groups = "drop") %>% 
    reshape_msd(include_type = FALSE)
  
  #pivot wide to subtract KP from GP (Total)
  df_vl <- df_vl %>% 
    pivot_wider(names_from = type,
                values_fill = 0) %>% 
    mutate(GenPop = Total - KeyPop,
           GenPop = ifelse(GenPop < 0, 0, GenPop)) %>% 
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
  
  #calculate VLC/S
  df_vl <- df_vl %>%
    mutate(vlc = tx_pvls_d/tx_curr_lag2,
           vls = tx_pvls/tx_pvls_d) %>% 
    filter(!is.nan(vlc))
  
  #limit to latest period
  df_vl <- filter(df_vl, period == max(period))
  
  #color
  df_viz <- df_vl %>% 
    select(-c(tx_pvls, tx_pvls_d, tx_curr_lag2)) %>% 
    mutate(fill_color = case_when(type == "KeyPop" ~ scooter,
                                  type == "AGYW" ~ genoa,
                                  TRUE ~ grey30k)) %>% 
    pivot_longer(c(vls, vlc), 
                 names_to = "indicator") %>% 
    mutate(group_viz = ifelse(group == "AGYW",
                          glue("**<span style='color:{genoa}'>AGYW</span> vs <span style='color:{grey30k}'>non-AGYW</span> {toupper(indicator)}**"),
                          glue("**<span style='color:{scooter}'>KeyPop</span> vs <span style='color:{grey30k}'>GenPop</span> {toupper(indicator)}**")))
  
  return(df_viz)
}
  

# VIZ ---------------------------------------------------------------------

viz_viral_load_kp_agyw <- function(df){
  
  if(is.null(df) || nrow(df) == 0)
    return(print(paste("No data available.")))
  
  ref_id <- "f5d17218" #id for adorning to plots, making it easier to find on GH
  
  vrsn <- 1 
  
  cap_note <- ifelse(nrow(df) > 21, "| Limited to the largest 20 TX_CURR PSNUs\n", "")
  
  #limit to 21 bars (overall + 20 psnus)
  v_top <- df %>% 
    filter(group == "AGYW") %>% 
    count(psnu, wt = tx_curr, sort = TRUE) %>% 
    slice_head(n = 21) %>% 
    pull(psnu)
  
  df <- filter(df, psnu %in% v_top) 
  
  #viz
  df %>% 
    ggplot(aes(value, fct_reorder(psnu, tx_curr, max, na.rm = TRUE), color = fill_color, group = psnu)) +
    geom_vline(xintercept = 0, color = "#D3D3D3") +
    geom_vline(xintercept = 1, color = "#D3D3D3", linetype = "dashed") +
    geom_line(color = "#d3d3d3", na.rm = TRUE) +
    geom_point(size = 2, color = "white", na.rm = TRUE) +
    geom_point(size = 2, alpha = .6, na.rm = TRUE) +
    scale_color_identity() +
    facet_wrap(~group_viz, nrow = 1) +
    scale_x_continuous(labels = scales::percent, name = NULL, 
                       limits = c(0,1.1), 
                       breaks = seq(0,1.1, by = .25),
                       oob = oob_squish) + 
    labs(x = NULL, y = NULL,
         subtitle = glue("{unique(df$period)} {unique(df$funding_agency)}/{unique(df$country)} VLC/S gaps between different population groups"),
         caption = glue("Note: VL capped at 110% {cap_note}{metadata_msd$caption} | USAID/OHA/SIEI | Ref id: {ref_id} v{vrsn}")) +
    si_style_xline() +
    theme(legend.position = "none",
          strip.text = element_markdown(),
          panel.spacing = unit(.5, "picas"))
  
}


