# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  TPT completion visuals
# REF ID:   a3e0423e 
# LICENSE:  MIT
# DATE:     2023-02-23
# UPDATED: 

# TB_PREV -----------------------------------------------------------------

prep_tpt <- function(df, cntry, agency) {
  
  df_tb_prev <- df %>% 
    filter(funding_agency == agency,
           country == cntry,
           indicator %in% c("TB_PREV", "TB_PREV_D"),
           standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) 
  
  #clean exit if no data
  if(nrow(df_tb_prev) == 0)
    return(NULL)
  
  df_tb_prev <- df_tb_prev %>% 
    group_by(country, funding_agency, snu1, indicator, fiscal_year) %>% 
    summarise(across(starts_with("qtr"), \(x) sum(x, na.rm = TRUE)), .groups = "drop") %>% 
    reshape_msd(include_type = FALSE)
  
  #clean exit if no data
  if(nrow(df_tb_prev) == 0)
    return(NULL)
  
  df_tb_prev <- df_tb_prev %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}") %>% 
    mutate(across(c(tb_prev, tb_prev_d), ~ na_if(., 0)),
           coverage = tb_prev / tb_prev_d,
           low_pnt = case_when(coverage < .85 ~ coverage),
           fill_color = ifelse(coverage < .85, old_rose, scooter))
  
  return(df_tb_prev)
  
  
  
}


viz_tpt <- function(df) {
  
  q <- glue::glue("Has 85%  TPT completion been achieved across all SNUs?") %>% toupper
  
  if(is.null(df) || nrow(df) == 0)
    return(dummy_plot(q))
  
  ref_id <- 'a3e0423e'
  vrsn <- 1 

  n_max <- 12
  
  v_tb_prev_lrg <- df %>% 
    filter(period == max(period)) %>% 
    arrange(desc(tb_prev_d)) %>% 
    slice_head(n = n_max) %>% 
    pull(snu1)
  
  cap_note <- ifelse(length(unique(df$snu1)) > n_max, glue("Limited to the largest TB_PREV_D SNUs |"), "")
  
  
  df %>% 
    filter(snu1 %in% v_tb_prev_lrg,
           !is.na(tb_prev_d)) %>% 
    ggplot(aes(period, coverage, group = snu1)) +
    geom_line(aes(group=snu1), color = suva_grey, alpha = .8, linewidth=1) +
    geom_point(aes(size = tb_prev_d, fill = fill_color), shape =21, color = "white") +
    facet_wrap(~fct_reorder(snu1, tb_prev_d, sum, na.rm = TRUE, .desc = TRUE))+
    geom_hline(yintercept = .85, linetype = "dashed") +
    geom_text(aes(label = percent(coverage, 1)), 
              vjust = -.75,
              color = matterhorn,
              #max.overlaps = 50, force = 10,
              family = "Source Sans Pro", na.rm = TRUE) +
    scale_y_continuous(label = percent_format(1)) +
    scale_fill_identity(aesthetics = c("fill", "color")) +
    expand_limits(y = 1) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL, 
           title = {q},
         subtitle = glue("{unique(df$funding_agency)}/{unique(df$country)} TPT completion rates amongst largest {length(v_tb_prev_lrg)} SNUs for TB_PREV_D"),
         caption = glue("Note:{cap_note} Points sized by TB_PREV_D
                        {metadata_msd$caption} | USAID/OHA/SIEI |  Ref id: {ref_id} v{vrsn}")) +
    si_style_ygrid() +
    theme(panel.spacing = unit(.5, "line"),
          legend.position = "none")

  
}


# step
# df_tb_prev %>%
#   filter(snu1 %in% v_tb_prev_lrg,
#          !is.na(tb_prev_d)) %>%
#   ggplot(aes(period, coverage, group = snu1)) +
#   geom_hline(yintercept = .85, linetype = "dashed") +
#   geom_step(na.rm = TRUE) +
#   geom_point(aes(y = low_pnt), shape = 21, size = 4,
#              fill = "white", color = old_rose,
#              na.rm = TRUE) +
#   geom_point(aes(color = fill_color), na.rm = TRUE) +
#   facet_wrap(~fct_reorder(snu1, tb_prev_d, sum, na.rm = TRUE, .desc = TRUE)) +
#   scale_y_continuous(label = percent_format(1)) +
#   scale_fill_identity(aesthetics = c("fill", "color")) +
#   labs(x = NULL, y = NULL,
#        #subtitle = glue("USAID saw signficant drps in TB_PREV coverage in {metadata_msd$curr_pd}"),
#        subtitle = glue("TPT completion rates amongst largest {length(v_tb_prev_lrg)} regions for TB_PREV_D"),
#        caption = glue("Source: {metadata_msd$source}")) +
#   si_style_ygrid() +
#   theme(panel.spacing = unit(.5, "line"))











