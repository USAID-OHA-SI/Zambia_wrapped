# PROJECT: Q4 Data Review
# PURPOSE: Munge and Analysis of KP
# AUTHOR: Tim Essam | SI
# REF ID:   85d850d1
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
   cascade::get_file_metadata()
  
  # REF ID for plots
    ref_id <- "85d850d1"
    
  # Functions  
  

# LOAD DATA ============================================================================  

  msd <- read_msd(file_path)

# MUNGE ============================================================================
  
  # FOCUSED on KP disags
  df_kp <- msd %>% 
      filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "HTS_RECENT", 
                              "KP_PREV", "PrEP_CT", "PrEP_CURR", "PrEP_NEW",
                              "TX_CURR", "TX_NEW", "TX_PVLS"),
             standardizeddisaggregate %in% c("KeyPop", "KeyPop/HIVStatus", 
                                             "KeyPopAbr", "KeyPop/Result")) %>% 
      clean_indicator() %>% 
      fix_mech_names() %>% 
      mutate(snu1 = str_remove_all(snu1, " Province"))
    
  df_ovc <- msd %>% 
    filter(indicator %in% c("OVC_SERV_UNDER_18", "OVC_HIVSTAT", "OVC_SERV",
                            "OVC_SERV_ACTIVE", "OVC_SERV_GRADUATED", "OVC_SERV_OVER_18",
                            "OVC_SERV_UNDER_18", "PP_PREV"),
           standardizeddisaggregate %in% c("Total Denominator", "Total Numerator", 
                                           "Age/Sex/ReportedStatus",
                                           "Age/Sex/DREAMS", "Age/Sex/Preventive",
                                           "Age/Sex/ProgramStatus", "Age/Sex/ProgramStatusCaregiver",
                                           "ProgramStatus", "TransferExit",
                                           "Age/Sex", "PopulationPriorityType", "Status"),
           fiscal_year == metadata$curr_fy) %>% 
    clean_indicator() %>% 
    fix_mech_names()
    
  
# VIZ ============================================================================

  gd_id <- "1r3rwCCUw3FeSeNLBwolmJ9citMilrlT4u_9jpVuUW8Y"  
    
  df_kp %>% 
      filter(fiscal_year == metadata$curr_fy) %>% 
      group_by(mech_code, mech_name, fiscal_year, indicator, otherdisaggregate, standardizeddisaggregate) %>% 
      summarise(across(matches("targ|qtr"), sum, na.rm = T)) %>% 
      relocate(targets, .after = qtr4) %>%
      googlesheets4::sheet_write(ss = gd_id, sheet = "KP_data")

  df_ovc %>% 
    group_by(mech_code, mech_name, fiscal_year, indicator, otherdisaggregate, standardizeddisaggregate) %>% 
    summarise(across(matches("targ|qtr"), sum, na.rm = T)) %>% 
    relocate(targets, .after = qtr4) %>% 
  googlesheets4::sheet_write(ss = gd_id, sheet = "OVC_data")
  
# SPINDOWN ============================================================================

