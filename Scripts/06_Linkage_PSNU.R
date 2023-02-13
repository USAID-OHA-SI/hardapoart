# PROJECT:  hardapoart
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  to visualize linkage across psnus, gaps for priority pops
# REF ID:   f6f26589 
# LICENSE:  MIT
# DATE:     2023-02-07
# UPDATED:  2023-02-10


# MUNGE -------------------------------------------------------------------


  prep_linkage_psnu <- function(df, cntry, agency, ...){
    
    #clean exit if no data
    if(cntry %ni% unique(df$country) | agency %ni% unique(df$funding_agency))
      return(NULL)
    
    #limit dataset to relevant indicators
    df_filtered <- df %>% 
      dplyr::filter(indicator %in% c("HTS_TST_POS", "TX_NEW", "HTS_TST"), 
             standardizeddisaggregate == "Total Numerator",
             funding_agency == agency,
             country == cntry) 
    
    #verify filter worked
    df_filtered %>% 
      assertr::verify(indicator %in% c("HTS_TST_POS", "TX_NEW", "HTS_TST") &
                        standardizeddisaggregate == "Total Numerator" &
                        funding_agency == agency &
                        country == cntry, 
                      error_fun = err_text(glue::glue("Error: {.df} has not been filtered correctly. 
                                               Please check the first filter in prep_psnu_linkage().")), 
                      description = glue::glue("Verify that the filters worked")) 
    
    #bind in duplicative rows to serve as overall total for plot
    df_filtered <- df_filtered %>% 
      dplyr::bind_rows(df_filtered %>% 
                         dplyr::mutate(psnu = "OVERALL"))
      
    #aggregate df tp psnu lvl for plotting
    df_reshaped <- df_filtered %>%
      dplyr::group_by(fiscal_year, country, funding_agency, psnu, indicator) %>% 
      dplyr::summarise(across(targets:qtr4, \(x) sum(x, na.rm = T)), 
                .groups = "drop") %>% 
      gophr::reshape_msd(direction ="semi-wide") 
    
    #verify reshape
    df_reshaped %>% 
      assertr::verify("period" %in% names(df_reshaped), 
                      error_fun = err_text(glue::glue("Error: {.df} has not been reshaped correctly and the period column does not exist. 
                                               Please check reshape_msd in prep_psnu_linkage().")), 
                      description = glue::glue("Verify that reshape_md worked"))
    
    #fill targets and remove 
    df_reshaped <- df_reshaped %>%
      dplyr::group_by(indicator) %>% 
      tidyr::fill(targets, .direction = "down") %>% 
      dplyr::filter(period == metadata_msd$curr_pd) %>% 
      dplyr::select(-targets)
    
    #verify filter to last period
    df_reshaped %>% 
      assertr::verify(period == metadata_msd$curr_pd & nchar(period) != 4, 
                      error_fun = err_text(glue::glue("Error: df_linkage_psnu has not been filtered correctly. 
                                               Please check the last filter in prep_psnu_linkage().")), 
                      description = glue::glue("Verify that last time period filtering worked")) 
    
    #create proxy linkage
    df_link <- df_reshaped %>%
      tidyr::pivot_wider(names_from = indicator, values_from = results) %>% 
      dplyr::mutate(linkage = TX_NEW / HTS_TST_POS)
    
    
    #adjust color for plot
    df_link <- df_link %>% 
      dplyr::mutate(fill_color = ifelse(psnu == "OVERALL", glitr::scooter, glitr::scooter_med))
    return(df_link)
    
  }
  
# VIZ --------------------------------------------------------------------------
  
  viz_linkage_psnu <- function(df){
    
    
    if(is.null(df))
      return(print(paste("No data available.")))
    
    ref_id <- "f6f26589"
    vrsn <- 1 
    
    cap_note <- ifelse(nrow(df) > 21, "Note: Limited to the largest 20 PSNUs\n", "")
    
    #limit to 21 bars (overall + 20 psnus)
      df <- df %>% 
        dplyr::slice_max(order_by = HTS_TST_POS, n = 21)
    
    df %>%
      ggplot(aes(linkage, forcats::fct_reorder(psnu, linkage), fill = fill_color)) +
      geom_vline(aes(xintercept = .95), linetype = "dashed", color = glitr::matterhorn) +
      geom_col() +
      geom_text(aes(label = percent(linkage, 1)), 
                size = 9/.pt, hjust = -.1,
                family = "Source Sans Pro", 
                color = glitr::matterhorn, 
                vjust = 0) +
      expand_limits(x = c(0, 1.1)) + 
      coord_cartesian(clip = "off") +
      scale_x_continuous(expand = c(.005, .005)) +
      scale_fill_identity() +
      si_style_nolines() +
      labs(subtitle = glue("{unique(df$funding_agency)}/{unique(df$country)} {metadata_msd$curr_pd} Proxy Linkage"),
           x = NULL, y = NULL,
           caption = glue("{cap_note}{metadata_msd$caption} | USAID | Ref id: {ref_id} v{vrsn}")) +
      theme(legend.position = "none",
            axis.text.x = element_blank())
 
  
  }