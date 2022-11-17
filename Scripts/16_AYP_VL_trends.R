# PROJECT: AMARA's POART DREAMS
# PURPOSE: Munge and Analysis of
# AUTHOR: Tim Essam | SI
# REF ID:   820e4a95
# LICENSE: MIT
# DATE: 2022-11-16
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
  
  # REF ID for plots
    ref_id <- "820e4a95"
    
  # Functions  
  source("Scripts/99_custom_functions.R")

# LOAD DATA ============================================================================  

  df_msd <- read_msd(file_path) 
  

# MUNGE ============================================================================
  
  #  Munge VLC by 15-19, 20-24 M/F
  df_vlc <- 
      df_msd %>% 
      filter(ageasentered %in% c("15-19", "20-24")) %>% 
      create_vl_df(ageasentered, sex) %>% 
      pivot_longer(cols = c(vlc, vls), 
                   names_to = "VL", 
                   values_to = "value") %>% 
      mutate(age_sex = str_c(ageasentered, sex, sep = " "))
      
  
# VIZ ============================================================================

  #  Create VIZ with two facets, 1 summarizing VLC one summarizing VLS
    df_vlc %>% 
      mutate(labels = case_when(
        period == metadata$curr_pd & age_sex == "15-19 Female" & VL == "vls" ~ "Viral Load\nSuppression",
        period == metadata$curr_pd & age_sex == "15-19 Female" & VL == "vlc" ~ "Viral Load\nCoverage",
        TRUE ~ NA_character_
      )) %>% 
      ggplot(aes(x = period, y = value, group = VL, color = VL, fill = VL)) +
      geom_line(size = 1) +
      geom_point(shape = 21, size = 3, color = "white", stroke = 0.25) +
      geom_text(aes(label = percent(value, 1)), size = 9/.pt,
                family = "Source Sans Pro", 
                vjust = -1) +
      geom_text(aes(label = labels), size = 9/.pt,
                family = "Source Sans Pro", 
                hjust = -0.2) +
    facet_wrap(~age_sex) +
      si_style_ygrid() +
      scale_y_continuous(labels = percent) +
      scale_color_manual(values = c("vlc" = denim, "vls" = burnt_sienna)) +
      scale_fill_manual(values = c("vlc" = denim, "vls" = burnt_sienna)) +
      scale_x_discrete(breaks = every_nth(n = 2)) +
      labs(x = NULL, y = NULL, caption = glue("{metadata$caption}"),
           title = "VIRAL LOAD COVERAGE INCREASED STEADILY ACROSS ALL AYP SEX/AGE BANDS IN FY22") +
      expand_limits(x = c(1, 10), y = c(0.6,1.0)) +
      theme(legend.position = "none")
    
    si_save("Graphics/AYP_VL_trends.svg", scale = 1.25)

# SPINDOWN ============================================================================

