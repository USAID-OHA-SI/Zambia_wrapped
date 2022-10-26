# PROJECT: Inspection of BOBS annual data provided by E4H
# PURPOSE: Munge and Analysis of BOBS data
# AUTHOR: Tim Essam | SI
# REF ID:   1ecdf8b1
# LICENSE: MIT
# DATE: 2022-10-25
# NOTES: Tim Essam | SI

# LOCALS & SETUP ============================================================================

  # Libraries
    library(gagglr)
    library(readxl)
    library(tidyverse)
    library(scales)
    library(sf)
    library(extrafont)
    library(tidytext)
    library(patchwork)
    library(ggtext)
    library(lubridate)
    
    
  # SI specific paths/functions  
    load_secrets()
    file_path <- "Data/BOB_Oct 21-Aug 22.xlsx"
    
    merdata <- file.path(glamr::si_path("path_msd"))
    site_path <- return_latest(folderpath = merdata, 
                               pattern = "Site_IM_FY20-23_20220812_v1_1_Zambia.zip")
  
  
  # REF ID for plots
    ref_id <- "1ecdf8b1"
    
  # Functions  
    source("Scripts/99_custom_functions.r")
    
    
  

# LOAD DATA ============================================================================  

    # df_sites <- read_msd(site_path) %>% filter(fiscal_year == 2022)
    # df_sites_roster <- df_sites %>% distinct(sitename, orgunituid)
    
    df_bobs <- read_excel(file_path) %>% 
      rename(facility = `facility name`,
             country = Country,
             province = Province,
             district = District,
             ip = `Implementing Partners`,
             indicator = Data,
             orgunituid = orgunitid)
    names(df_bobs)
    
    ctry_list <- grabr::get_outable() %>% 
      select(country, country_uid, facility_lvl, psnu_lvl) %>% 
      filter(country == "Zambia")
    
    #pull site coordinates
    df_coords <- ctry_list %>%
      pmap_dfr(~pull_coords(..1, ..2, ..3, ..4)) 
    
    # Extract orgunituid list from Bobs data
    df_site_coords <- df_bobs %>% 
      distinct(orgunituid, facility, ip) %>% 
      left_join(., df_coords) %>% 
      mutate(flag = is.na(latitude))
    
    df_site_coords %>% 
      write_csv(., "Dataout/zmb_sites_coordinates.csv", na = "")

# MUNGE ============================================================================
  
    # How many facilities do not have lat/lon?
    df_site_coords %>% filter(is.na(latitude)) %>% tally()
    
    # List of facilities without matching orgunituid 
    # 11 facilities that need to be investigated
    df_site_coords %>% filter(is.na(latitude), is.na(psnuuid))
    
    # How many mechs reported?
    df_bobs %>% distinct(ip)
    
    df_bobs <- 
      df_bobs %>% mutate(mech_code = case_when(
      ip == "MECH" ~ "82075",
      ip == "SAFE" ~ "17413",
      ip == "Discover Health" ~ "17399",
      ip == "ZAM-Health" ~ "86412",
      ip == "STOP GBV Project" ~ "18487",
      ),
      month_num = match(month, month.name), 
      bobs_date = make_datetime(year = year, month = month_num, day = 1),
      latest_date = ifelse(bobs_date == max(bobs_date), 1, 0),
      indicator = case_when(
        str_detect(indicator, "Denom") ~ "TX_PVLS_D",
        str_detect(indicator, "Numera") ~ "TX_PVLS_N",
        str_detect(indicator, "Cover") ~ "VLC",
        str_detect(indicator, "Supp") ~ "VLS",
        TRUE ~ indicator
        )
      )
  
    # Derived metrics
    df_bobs_cmetrics <- 
      df_bobs %>% 
      pivot_wider(names_from = indicator, values_from = value)
    
    
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================

    df_bobs %>% write_csv("Dataout/bobs_sample.csv")

    df_bobs %>% names()

    df_bobs %>% 
      filter(ip == "SAFE") %>% 
      group_by(bobs_date, indicator, ip) %>% 
      summarise(tot = sum(value, na.rm = T)) %>% 
      spread(bobs_date, tot)
    