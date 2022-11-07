# PROJECT: Ad hoc requests for Q4 data based on review
# PURPOSE: Munge and Analysis of Q4 Data
# AUTHOR: Tim Essam | SI
# REF ID:   e551032c
# LICENSE: MIT
# DATE: 2022-11-07
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
      pattern = "Genie-PSNUByIMs-Zambia")
    
    old_data <- return_latest(folderpath = merdata,
                              pattern = "PSNU_IM_FY18-21_20201218_v2_1_Zambia.zip")
      
  # Grab metadata
   get_metadata(file_path)
  
  # REF ID for plots
    ref_id <- "e551032c"
    
  # Functions  
  

# LOAD DATA ============================================================================  

  df_msd <- read_msd(file_path) %>% filter(indicator == "TB_PREV", fiscal_year >= 2021)
  df_old_msd <-  read_msd(old_data) %>% 
    filter(indicator == "TB_PREV", operatingunit == "Zambia", fiscal_year < 2021)

# MUNGE ============================================================================
  
  df_tb <- df_msd %>% clean_indicator() %>% 
    filter(standardizeddisaggregate %in% c("Total Numerator")) %>% 
    group_by(funding_agency, fiscal_year, indicator) %>% 
    summarise(across(matches("qtr|targ"), sum, na.rm = T)) %>% 
    bind_rows(df_old_msd %>% clean_indicator() %>% 
                filter(standardizeddisaggregate == "Total Numerator") %>% 
                group_by(funding_agency, fiscal_year, indicator) %>% 
                summarise(across(matches("qtr|targ"), sum, na.rm = T))
              ) %>% 
    filter(funding_agency == "USAID") %>% 
    arrange(fiscal_year) %>% 
    reshape_msd(direction = "quarters")
  
  df_tb_viz <- 
    df_tb %>% bind_rows(
  df_msd %>% clean_indicator() %>% filter(fiscal_year == 2023, funding_agency == "USAID", 
                                          standardizeddisaggregate %in% c("Total Numerator")) %>% 
    group_by(funding_agency, fiscal_year, indicator) %>% 
    summarise(across(matches("qtr|targ"), sum, na.rm = T)) %>% 
    select(funding_agency, fiscal_year, targets, indicator) 
  ) %>% 
    mutate(period = ifelse(fiscal_year == 2023, "FY23Q4", period)) %>% 
    filter(str_detect(period, "Q4")) %>% 
    calc_achievement() %>% 
    adorn_achievement()
  
# VIZ ============================================================================

  df_tb_viz %>% 
    ggplot(aes(factor(fiscal_year))) +
    geom_col(aes(y = targets), fill = grey20k, width = .5) +
    geom_col(aes(y = results_cumulative), width = .5, 
             position = position_nudge(x = 0.15), fill = scooter_med) +
    geom_label(aes(y = results_cumulative, label = percent(achievement_qtrly)),  
               position = position_nudge(x = 0.15), 
               vjust = -.25,
               alpha = 0.5) +
    scale_y_continuous(labels = label_number_si()) +
    labs(x = NULL, y = NULL, 
         title = "USAID TB_PREV_N TARGETS AND RESULTS 2018 - 2023", 
         caption = glue("{metadata$caption}")) +
    si_style_ygrid()
  
  si_save("Images/TB_prev_trends.png")

# SPINDOWN ============================================================================

