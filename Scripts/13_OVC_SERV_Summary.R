# PROJECT: POART Dreaming
# PURPOSE: Munge and Analysis of POART Data for AR
# AUTHOR: Tim Essam | SI
# REF ID:   cbf4c1b2
# LICENSE: MIT
# DATE: 2022-11-14
# NOTES: Tim Essam | SI

# LOCALS & SETUP ============================================================================

  # Libraries
    library(gagglr)
    library(tidyverse)
    library(scales)
    library(sf)
    library(extrafont)
    library(tidytext)
    library(patchwork)
    library(ggtext)
    library(glue)
    
    
  # SI specific paths/functions  
    load_secrets()
    merdata <- file.path(glamr::si_path("path_msd"))
    file_path <- return_latest(folderpath = merdata,
      pattern = "Genie-PSNUByIMs-Zambia-Daily-2022-11-14")
      
  # Grab metadata
   get_metadata(file_path)
   
  # GENIE DETAILS
   # DATIM data as of: 11/13/2022, 23:31:49 UTC
   # Genie report updated: 11/14/2022, 04:36:01 UTC
   # Current period(s): 2021 Target, 2021 Q1, 2021 Q2, 2021 Q3, 2021 Q4, 
   # 2022 Target, 2022 Q1, 2022 Q2, 2022 Q3, 2022 Q4, 2023 Target
  
  # REF ID for plots
    ref_id <- "cbf4c1b2"
    
  # Functions  
  munge_ovc <- function(...){
    df_msd %>% 
      clean_psnu() %>% 
      filter(str_detect(indicator, "OVC_SERV"), standardizeddisaggregate == "Total Numerator") %>% 
      group_by(fiscal_year, indicator, ...) %>% 
      summarize(across(c(targets, cumulative), sum, na.rm = T)) %>% 
      ungroup()
  }
    
    

# LOAD DATA ============================================================================  

  df_msd <- read_msd(file_path) 
  

# OVC_SERVE ============================================================================
  
  df_ovc_ou <- munge_ovc() %>% adorn_achievement()
    
  df_ovc_psnu <- munge_ovc(psnu) %>% adorn_achievement()
  
# VIZ ============================================================================

  # Overall and PSNU VIZ combined show results to targets for FY22
  top <- df_ovc_ou %>% 
    filter(indicator %in% c("OVC_SERV", "OVC_SERV_UNDER_18"), fiscal_year == metadata$curr_fy) %>% 
    ggplot(aes(y = indicator, group = indicator)) +
    #geom_linerange(aes(xmin = 0, xmax = targets), linewidth = 4,) +
    geom_segment(aes(x = 0, xend = targets, yend = indicator), linewidth = 5, lineend = "round", color = grey20k)+
    geom_point(aes(x = cumulative, fill = achv_color), shape = 21, size = 5) +
    geom_text(aes(x = cumulative, label = percent(achievement, 1)),
              vjust = -1,
              size = 10/.pt,
              color = grey90k) +
    facet_wrap(~indicator, scales = "free") +
    scale_fill_identity() +
    scale_x_continuous(label = scales::label_number_si()) +
    si_style_xgrid() +
    labs(x = NULL, y = NULL) +
    theme(axis.text.y = element_blank())
  
bottom <- df_ovc_psnu %>% 
    filter(indicator %in% c("OVC_SERV", "OVC_SERV_UNDER_18"), fiscal_year == metadata$curr_fy,
           psnu != "Katete") %>%
    mutate(high_achv = ifelse(achievement > .90, 0, 1)) %>% 
    group_by(indicator, high_achv) %>% 
    mutate(psnu_order = fct_reorder(psnu, cumulative)) %>% 
    ungroup() %>% 
    ggplot(aes(y = psnu_order, group = indicator)) +
    #geom_linerange(aes(xmin = 0, xmax = targets), linewidth = 4,) +
    geom_segment(aes(x = 0, xend = targets, yend = psnu_order), linewidth = 3, lineend = "round", color = grey20k)+
    geom_vline(xintercept = -10, size = 3, color = "white") +
    geom_vline(xintercept = -0, size = 0.5, color = grey90k) +
    geom_point(aes(x = cumulative, fill = achv_color), shape = 21, size = 3, stroke = 0.25) +
    geom_text(aes(x = cumulative, label = percent(achievement, 1)),
              hjust = -.25,
              size = 8/.pt,
              color = grey90k) +
    facet_grid(high_achv ~ indicator, scales = "free", space = "free") +
    scale_fill_identity() +
    scale_x_continuous(label = scales::label_number_si()) +
    si_style_xgrid() +
    labs(x = NULL, y = NULL, caption = glue::glue("{metadata$source}"))  
 
 top / bottom +
   plot_layout(heights = c(1, 4)) +
   plot_annotation(title = "ZAMBIA REACHED 108% OVERALL OVC_SERV TARGETS IN FY22")
  
 si_save("Images/OVC_summary.svg", scale = 1.25)
  
 
 # TRENDS for OVC U18 + O18
 df_ovc_ou %>% 
   filter(indicator %in% c("OVC_SERV", "OVC_SERV_UNDER_18", "OVC_SERV_OVER_18")) %>% 
   mutate(indic_order = fct_relevel(indicator, c("OVC_SERV", "OVC_SERV_UNDER_18", "OVC_SERV_OVER_18"))) %>% 
   ggplot(aes(x = factor(fiscal_year))) +
    geom_col(aes(y = targets), fill = grey20k, width = 0.5, position = position_nudge(x = -0.2)) +
   geom_col(aes(y = cumulative), fill = scooter_med, width = 0.5, ) +
   geom_text(data = . %>% filter(fiscal_year != 2023), 
              aes(y = cumulative, label = percent(achievement, 1)), size = 10/.pt, 
              family = "Source Sans Pro",
              vjust = -0.15) +
   facet_wrap(~indic_order) +
   scale_y_continuous(labels = label_number_si(), expand = c(0, 0.2), 
                      limits = c(0, 650000)) +
   si_style_ygrid() +
   labs(x = NULL, y = NULL, 
        caption = glue("{metadata$caption}"),
        title = "ZAMBIA ACHIEVED ALL OVC_SERV TARGETS IN FY22") 
 
 si_save("Images/OVC_SERV_trends.png", scale = 1.25)
 

# SPINDOWN ============================================================================

