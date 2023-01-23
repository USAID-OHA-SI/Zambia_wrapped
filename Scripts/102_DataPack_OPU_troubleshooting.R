# PROJECT: Datapack OPU Issue
# PURPOSE: Munge and Analysis of Datapack looking at OPU issue
# AUTHOR: Tim Essam | SI
# REF ID:   5f987b0f
# LICENSE: MIT
# DATE: 2023-01-18
# NOTES: Tim Essam | SI

# NOTES: 

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
    library(tameDP)
    
    
  # SI specific paths/functions  
    file_path <- "../../../Downloads/OPU Data Pack_Zambia_20221118_DP Error resolve.xlsx"
    file_path2 <- "../../../Downloads/OPU Data Pack_Zambia_20221118.xlsx"
    file_path3 <- "../../../Downloads/OPU Data Pack_Zambia_20221118_DP Error resolve_test2.xlsx"
      
  # Grab metadata
   get_metadata(file_path)
  
  # REF ID for plots
    ref_id <- "5f987b0f"
    
  # Functions  
  

# LOAD DATA ============================================================================  

  df <- tame_dp(filepath = file_path, type = "PSNUxIM")
  df2 <- tame_dp(filepath = file_path2, type = "PSNUxIM")
  df3 <- tame_dp(filepath = file_path3, type = "PSNUxIM")

  df %>% count(mech_code) %>% prinf()

# MUNGE ============================================================================
  
  #  Mechs of interest 86412, 17400, 86411
  df2 %>% filter(mech_code %in% c(86412, 17400, 86411)) %>% 
    count(mech_code, indicator) %>% prinf()
  
  df3 %>% filter(mech_code %in% c(86412, 17400, 86411)) %>% 
    count(mech_code, indicator) %>% prinf()
  
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================

