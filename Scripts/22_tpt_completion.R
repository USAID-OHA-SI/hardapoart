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
           standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>% 
    # clean_indicator() %>% 
    group_by(country, funding_agency, snu1, indicator, fiscal_year) %>% 
    summarise(across(starts_with("qtr"), \(x) sum(x, na.rm = TRUE)), .groups = "drop") %>% 
    reshape_msd(include_type = FALSE) %>% 
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

  
  v_tb_prev_lrg <- df %>% 
    filter(period == max(period)) %>% 
    arrange(desc(tb_prev_d)) %>% 
    mutate(cumsum = cumsum(tb_prev_d)/sum(tb_prev_d)) %>% 
    slice_head(n = 11) %>% 
    pull(snu1)
  
  cap_note <- ifelse(nrow(df) > 21, glue("Note: Points sized by TB_PREV_D \n", ""))
  
  
  df %>% 
    filter(snu1 %in% v_tb_prev_lrg,
           !is.na(tb_prev_d)) %>% 
    ggplot(aes(period, coverage, group = snu1)) +
    geom_point(aes(size = tb_prev_d, color = fill_color)) +
    geom_line(aes(group=snu1, color = fill_color), linewidth=1) +
    facet_wrap(~fct_reorder(snu1, tb_prev_d, sum, na.rm = TRUE, .desc = TRUE))+
    geom_hline(yintercept = .85, linetype = "dashed") +
    geom_text(aes(label = percent(coverage, 1)), 
              vjust = -.75,
              #color = trolley_grey,
              #max.overlaps = 50, force = 10,
              family = "Source Sans Pro", na.rm = TRUE) +
    scale_y_continuous(label = percent_format(1), limits = c(0.5,1)) +
    scale_fill_identity(aesthetics = c("fill", "color")) +
    labs(x = NULL, y = NULL, 
           title = {q},
         subtitle = glue("{unique(df$funding_agency)}/{unique(df$country)} TPT completion rates amongst largest {length(v_tb_prev_lrg)} SNUs for TB_PREV_D"),
         caption = glue("{cap_note}
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











