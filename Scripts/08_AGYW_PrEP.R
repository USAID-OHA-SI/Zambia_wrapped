# PROJECT: Q4 Review
# PURPOSE: Munge and Analysis of AGYW PreP_NEW
# AUTHOR: Tim Essam | SI
# REF ID:   d9bc6c3a
# LICENSE: MIT
# DATE: 2022-10-26
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
      
  # Grab metadata
    get_metadata(file_path)
  
  # REF ID for plots
    ref_id <- "d9bc6c3a"
    
  # Functions  
  

# LOAD DATA ============================================================================  

  df_prep <- read_msd(file_path) %>% 
      filter(indicator == "PrEP_NEW",
             funding_agency == "USAID")

# PREP_NEW AGYW ============================================================================
  
    #Comparison: 15-24 year females (AGYW) compared to 25+ females
    df_prep <- 
      df_prep %>% 
      filter(sex == "Female") %>% 
      mutate(age = case_when(
        ageasentered %in% c("15-19", "20-24") ~ "AGYW (15-24)",
        TRUE ~ "Females 25+"
      )
      ) %>% 
      group_by(age, indicator, fiscal_year) %>% 
      summarise(across(c(targets, starts_with("cum")), sum, na.rm = TRUE), .groups = "drop") %>% 
      mutate(achv = cumulative / targets,
             fill_color = ifelse(age == "AGYW (15-24)", golden_sand, denim))
    
    
    width <- .6  
    offset <- 0.15
    
    df_prep %>% 
      ggplot(aes(x = as.factor(fiscal_year), group = fill_color)) +
      geom_col(aes(y = targets), fill = grey10k, width = width ) +
      geom_col(aes(y = cumulative, fill = fill_color), width = width,
               position = position_nudge(x = offset)) +
      geom_text(aes(y = cumulative, label = percent(achv, 1)), 
                size = 10/.pt, 
                family = "Source Sans Pro",
                position = position_nudge(x = offset), 
                vjust = -0.2) +
      facet_wrap(~age) +
      si_style_ygrid(facet_space = 0.75) +
      scale_y_continuous(labels = comma, sec.axis = dup_axis()) +
      scale_fill_identity() +
      labs(x = NULL, y = NULL, 
           caption = glue("Source: {metadata$source}"),
           title = "FY21 - FY22 PrEP INITATIONS AMONG AGYW AND FEMALES 25+")
    
    si_save(glue("Graphics/{metadata$curr_pd}_ZAM_PrEP_NEW_AGYW.svg"))  
  
# KP ============================================================================

    df_kp <- 
      df_prev %>% 
      filter(standardizeddisaggregate == "KeyPopAbr",
             funding_agency == "USAID") %>%
      group_by(otherdisaggregate, fiscal_year, indicator) %>% 
      summarise(across(c(targets, starts_with("qtr")), sum, na.rm = TRUE), .groups = "drop") %>% 
      reshape_msd("quarters") %>% 
    select(-results) %>% 
      rename(results = results_cumulative)

# KP PLOT  ============================================================================

    width <- .6  
    offset <- 0.15
    ymax = 3.6e3
    ymax2 = 1000
    
    df_kp %>%
      filter(otherdisaggregate != "PWID") %>% 
      mutate(ymax = ifelse(otherdisaggregate %in% c("FSW", "MSM"), ymax, ymax2),
             achv = case_when(
               period %in% c("FY21Q4", metadata$curr_pd) ~ results/targets, 
               TRUE ~ NA_real_
             )
      ) %>% 
      group_by(otherdisaggregate) %>% 
      mutate(qtrs_run = row_number()) %>% 
      ungroup() %>% 
      filter(qtrs_run > 0) %>% 
      ggplot(aes(x = period)) +
      geom_blank(aes(ymax = ymax)) +
      geom_col(aes(y = targets), fill = grey10k, width = width ) +
      geom_col(aes(y = results), fill = scooter_med, width = width,
               position = position_nudge(x = offset)) +
      geom_text(aes(y = results, label = percent(achv, 1)), 
                size = 10/.pt, 
                family = "Source Sans Pro",
                position = position_nudge(x = offset), 
                vjust = 0.1)+
      facet_wrap(~otherdisaggregate, scales = "free_y") +
      si_style_ygrid(facet_space = 0.75) +
      scale_y_continuous(labels = comma) +
      labs(x = NULL, y = NULL, 
           caption = glue("Source: {metadata$source}"))
    
    si_save(glue("Graphics/{curr_pd}_ZAM_PrEP_NEW_KP.svg"))  
