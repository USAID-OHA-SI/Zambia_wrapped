# PROJECT: Musonda's request for site level TX_CURR for SAFE
# PURPOSE: Munge and Analysis of SITE LEVEL MSD
# AUTHOR: Tim Essam | SI
# REF ID:   d1a2385e
# LICENSE: MIT
# DATE: 2022-11-28
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
      pattern = "Site_IM_FY20-23_20221114.*_Zambia")
      
  # Grab metadata
   get_metadata(file_path)
  
  # REF ID for plots
    ref_id <- "d1a2385e"
    
  # Functions  
    source("Scripts/99_custom_functions.r")

# LOAD DATA ============================================================================  

  df_msd <- read_msd(file_path) %>% 
      filter(indicator == "TX_CURR", 
             standardizeddisaggregate == "Age/Sex/HIVStatus", 
             snu1 == "Copperbelt Province", 
             mech_name == "SAFE") 
    
# MUNGE ============================================================================
  
  # TX_CURR data frame by site, we'll be nice and include TX_CURR under 15, over 15 and ALL
  df_tx <- df_msd %>% 
      group_by(facility, facilityuid, snu1, psnu, trendscoarse, fiscal_year, indicator, mech_name, funding_agency) %>% 
      summarise(across(matches("qtr"), sum, na.rm = T), .groups = "drop") %>% 
      reshape_msd() %>% 
      select(-c(indicator, period_type)) %>% 
      pivot_wider(names_from = trendscoarse, values_from = value, names_glue = "TX_CURR_{trendscoarse}") %>% 
      mutate(TX_CURR_ALL = `TX_CURR_<15`+ `TX_CURR_15+`)
      
# SPINDOWN ============================================================================

  # Write it to a google sheet 
    gs_id <- "1SG6f4TcbmI3Hyn4c_njIQM3C3k1P5mIv4zId2EOgg6k"

    googlesheets4::write_sheet(df_tx, ss = gs_id, sheet = "TX_CURR")
        