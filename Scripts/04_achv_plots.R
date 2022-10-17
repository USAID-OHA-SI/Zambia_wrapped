# PROJECT: AYP Achievement by IP - table
# PURPOSE: Create FY23 Coverage maps for All of Zambia 
# AUTHOR: Tim Essam | SI
# REF ID:   702b8752
# LICENSE: MIT
# DATE: 2022-10-17
# NOTES: Tim Essam | SI

# LOCALS & SETUP ============================================================================

  # Libraries
  library(gagglr)
  library(tidyverse)
  library(scales)
  library(sf)
  library(glue)
  library(extrafont)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(selfdestructin5)
  library(gt)
  library(cascade) # Use dev version
  library(ggpattern)
  
  
  # SI specific paths/functions  
  load_secrets()
  merdata <- file.path(glamr::si_path("path_msd"))
  file_path <- return_latest(folderpath = merdata,
                             pattern = "PSNU_IM_FY20-23_20220916_v2_1_Zambia.zip")
  
  plhiv_path <- return_latest(folderpath = merdata,
                              pattern = "SUBNAT")

  # REF ID for plots
  ref_id <- "98614ee3"  
  
  # Grab metadata
  get_metadata(file_path)
  
  # Functions  
  source("Scripts/99_custom_functions.r")
  

# MUNGE -------------------------------------------------------------------

  df <- read_msd(file_path) %>% 
    fix_mech_names()
  
  # Looking for a ACHV table by IP across the following
  # indic_list <- c("PrEP_NEW", "HTS_TST_POS", "TX_CURR", "TX_NEW", "VLS", "VLC")
  
  df_achv <- df %>% 
    filter(indicator %in% c("PrEP_NEW", "HTS_TST_POS", "TX_NEW", "TX_PVLS", "TX_CURR"),
           standardizeddisaggregate == "Total Numerator", 
           fiscal_year == metadata$curr_fy, 
           funding_agency != "Dedup") %>% 
    group_by(mech_code, funding_agency, indicator, fiscal_year) %>% 
    summarise(across(matches("cumul|targ"), sum, na.rm = T), .groups = "drop") %>% 
    calc_achievement() %>% 
    adorn_achievement()
           
  df_vl <- df %>% 
    create_vl_df(mech_name, mech_code)
  