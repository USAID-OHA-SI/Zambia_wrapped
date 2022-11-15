# PROJECT: MEGAN's BESPOKE REQUESTS
# PURPOSE: Munge and Analysis of MSDs
# AUTHOR: Tim Essam | SI
# REF ID:   d0ae3bcb
# LICENSE: MIT
# DATE: 2022-11-15
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
    
    
  # SI specific paths/functions  
    load_secrets()
    merdata <- file.path(glamr::si_path("path_msd"))
    file_path <- return_latest(folderpath = merdata,
      pattern = "Genie-SiteByIMs-Zambia-Daily")
      
  # Grab metadata
   get_metadata(file_path)
  
  # REF ID for plots
    ref_id <- "d0ae3bcb"
    
  # Functions  
  source("Scripts/99_custom_functions.R")
  

# LOAD DATA ============================================================================  

  df_msd <- read_msd(file_path) 
  
  gd_id <- "1CBEp-0isNwJk-P_cJxJvTpN_HwO7UETNgHC2g623Gtw"
  gd_id_ovc <- "1hjziU5zXzk8rH9M_uFKouvmhLnlX4csnXzeesHF2ecI"

# MUNGE ============================================================================
  
# REQUESTS
  #  can I get a list of sites with >15 TXCURR and either blank or 0 <15 TXCURR again?
  # Can you please send me:
  #   1) <15 VLC and VLC results by district for Copperbelt, Northwestern, Central, Southern, Lusaka and Eastern, Lupula and Northern provinces. 
  #   2) <15 OVC_HIVSTAT_POS by district (for the above provinces)
  #   3) peds <15 TX_CURR by district (for the same provinces)  
  
  # Sites with TX_CURR either 0 or blank for <15s
    tx_df <- df_msd %>% filter(indicator == "TX_CURR",
                      standardizeddisaggregate == "Age/Sex/HIVStatus", 
                      fiscal_year == metadata$curr_fy, 
                      facility != "Data reported above Facility level", 
                      mech_name != "Dedup") %>% 
      group_by(facility, orgunituid, mech_name, psnu, snu1, fiscal_year, trendscoarse, indicator) %>% 
      summarise(across(starts_with("qtr"), sum, na.rm = T)) %>% 
      ungroup() %>% 
      filter(qtr4 == 0 | is.na(qtr4)) 
      
    googlesheets4::write_sheet(tx_df, ss = gd_id, sheet = "tx_df")
    
    
  # VLC results
   df_peds_vl <- 
     df_msd %>% 
      filter(trendscoarse == "<15") %>% 
      create_vl_df(snu1, trendscoarse) %>% 
      mutate(snu1 = str_remove_all(snu1, " Province")) %>% 
      filter(snu1 %ni% c("_Military Zambia", "Muchinga", "Western"))
   
   googlesheets4::write_sheet(df_peds_vl, ss = gd_id_ovc, sheet = "PEDS_VLS")
    
  df_ovc_hivstat <- 
    df_msd %>% 
    filter(indicator %in% c("TX_CURR", "OVC_HIVSTAT_POS"),
           standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Total Numerator"),
           fiscal_year == metadata$curr_fy) %>% 
    group_by(indicator, psnu, trendscoarse, snu1, fiscal_year) %>% 
    summarise(across(matches("qtr4"), sum, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(keep_flag = case_when(
      indicator == "OVC_HIVSTAT_POS" ~ 1,
      indicator == "TX_CURR" & trendscoarse == "<15" ~ 1,
      TRUE ~ 0
    )) %>% 
    filter(keep_flag == 1) %>% 
    select(-trendscoarse) %>% 
    pivot_wider(names_from = indicator, values_from = qtr4) %>% 
    filter(!is.na(OVC_HIVSTAT_POS)) %>% 
    mutate(proxy_coverage = OVC_HIVSTAT_POS / TX_CURR) %>% 
    mutate(snu1 = str_remove_all(snu1, " Province")) %>% 
    group_by(snu1) %>% 
    mutate(across(matches("OVC|TX"), sum, na.rm = T, .names = "SNU_{.col}"),
           snu_proxy_coverage = SNU_OVC_HIVSTAT_POS / SNU_TX_CURR) %>% 
    filter(snu1 %ni% c("_Military Zambia", "Muchinga", "Western")) %>% 
    group_by(snu1) %>% 
    mutate(keep_tab = row_number()) %>% 
    ungroup()
  
  googlesheets4::write_sheet(df_ovc_hivstat, ss = gd_id_ovc, sheet = "OVC_HIVSTAT_POS")
  

# VIZ ============================================================================

  #  PEDS VL
  df_peds_vl %>% 
  ggplot(aes(x = period, group = 1)) +
    geom_line(aes(y = vls), color = burnt_sienna) +
    geom_point(aes(y = vls), shape = 21, fill = burnt_sienna, size = 3,
               color = "white") +
    geom_line(aes(y = vlc), color = denim) +
    geom_point(aes(y = vlc), shape = 21, fill = denim, size = 3,
               color = "white") +
    geom_text(aes(y = vlc, label = percent(vlc, 1)), size = 9/.pt,
              family = "Source Sans Pro", color = denim, 
              vjust = -1) +
    geom_text(aes(y = vls, label = percent(vls, 1)), size = 9/.pt,
              family = "Source Sans Pro", color = burnt_sienna, 
              vjust = -1) +
    # annotate("text", x = 8.5, y = .97, label = "Viral Load\nSuppression",
    #          color = burnt_sienna, size = 10/.pt,
    #          hjust = 0.1) +
    # annotate("text", x = 8.5, y = .69, label = "Viral Load\nCoverage",
    #          color = denim, size = 10/.pt,
    #          hjust = 0.1) +
    si_style_ygrid(facet_space = 0.25) +
    expand_limits(x = c(1, 9), y = c(0.7,1.05)) +
    scale_x_discrete(labels = c("FY21Q1", "", "FY21Q3", "", 
                                "FY22Q1", "", "FY22Q3", "")) +
    theme(axis.text.y = element_blank()) +
    labs(x = NULL, y = NULL, title = "PEDIATRIC VIRAL LOAD COVERAGE & SUPPRESSION IN SELECT PROVINCES",
         caption = glue::glue("{metadata$caption}")) +
    facet_wrap(~snu1) 
  
  si_save("Images/ZMB_peds_vlc_trends.png", scale = 1.25)
  
  # PROX COVERAGE
  df_ovc_hivstat %>% 
    filter(keep_tab == 1) %>% 
    mutate(snu_order = fct_reorder(snu1, snu_proxy_coverage)) %>% 
    ggplot(aes(y = snu_order, x = snu_proxy_coverage)) +
    geom_segment(aes(x = 0, xend = snu_proxy_coverage, yend = snu_order), linewidth = 5, lineend = "round", color = grey20k)+
    geom_vline(xintercept = 1, size = 1, color = grey50k) +
    geom_point(aes(fill = snu_proxy_coverage), shape = 21, size = 5) +
    geom_text(aes(label = percent(snu_proxy_coverage, 1)), size = 11/.pt, 
                  family = "Source Sans Pro",
              vjust = -1)+
    scale_fill_viridis_c(direction = -1) +
    scale_x_continuous(labels = percent) +
    si_style_xgrid() +
    labs(x = "PROXY COVERAGE", y = NULL, title = "FY22 Q4 OVC PROXY COVERAGE BY PROVINCE", 
         caption = glue::glue("{metadata$caption}")) +
    theme(legend.position = "none")
   
  si_save("Images/ZMB_proxy_coverage.png", scale = 1.25)
  
 # What is the relationship between VLC & OVC PROXY COVERAGE?
  