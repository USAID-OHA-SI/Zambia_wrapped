# PROJECT: FY22 Q4 POART'ish review
# PURPOSE: Munge and Analysis of HTS data / modalities
# AUTHOR: Tim Essam | SI
# REF ID:   0bb2a8eb
# LICENSE: MIT
# DATE: 2022-10-14
# NOTES: Tim Essam | SI

# LOCALS & SETUP =========================================================================
    
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
                             pattern = "Genie-PSNUByIMs-Zambia")
  
  # Refid
  refid <- "0bb2a8eb"
  
  # Grab metadata
  get_metadata(file_path)
  
  # Functions  
  source("Scripts/99_custom_functions.r")
  

# LOAD DATA ============================================================================  

  df_msd <- read_msd(file_path) %>% 
    fix_mech_names() %>% 
    swap_targets()


# HTS MODALITY ------------------------------------------------------------

  # To plot USAID, reset mech_code to a constant
    munge_modality(df_msd %>% mutate(mech_code = 123456, mech_name = "USAID"), 
                 mech_code == 123456) %>% 
    plot_modality(.)
    si_save("Images/Index_testing_summary_by_modality")

  # Loop over key partners  
  partner_list <- df_msd %>% 
    filter(str_detect(mech_name, "Action|DISCOVER|SAFE|Zam Health")) %>% 
    distinct(mech_code) %>% pull()
  
  map(partner_list, ~batch_modality_plot(df_msd, .x))

# LINKAGE TRENDS ----------------------------------------------------------

   # Linkage Trends
 df_hts_base <- 
    df_msd %>% 
    filter(indicator %in% c("HTS_TST_POS", "TX_NEW", "HTS_TST"), 
           standardizeddisaggregate == "Total Numerator",
           fiscal_year <= metadata$curr_fy, 
           funding_agency == "USAID") %>% 
    group_by(indicator, fiscal_year) %>% 
    summarise(across(targets:qtr4, sum, na.rm = T), .groups = "drop") %>% 
    reshape_msd(direction ="semi-wide") %>% 
    group_by(indicator) %>% 
    fill(targets, .direction = "down") %>% 
    filter(nchar(period) != 4) 
 
 df_hts_tgt <- df_hts_base %>% select(-results) %>% 
   pivot_wider(names_from = indicator, values_from = targets) %>% 
   mutate(linkage = TX_NEW / HTS_TST_POS)
 
 df_linkage <- 
   df_hts_base %>% 
   select(-targets) %>% 
    pivot_wider(names_from = indicator, values_from = results) %>% 
    mutate(linkage = TX_NEW / HTS_TST_POS)
    

 
 
# VIZ ===========================================================================



  

 # Linkage visual w/ targets
 bottom_hts <- df_linkage %>% 
   ggplot(aes(x = period)) +
   # geom_col(data = df_hts_tgt, aes(y = HTS_TST_POS), fill = "#efe9ed",
   #          position = position_nudge(x = 0.15), width = 0.5) +
   # geom_col(data = df_hts_tgt, aes(y = TX_NEW), fill = "#f8ead9", 
   #          position = position_nudge(x = -0.15), width = 0.5) +
   geom_col(aes(y = HTS_TST_POS), fill = "#855C75",
            position = position_nudge(x = 0.1), width = 0.5) +
   geom_col(aes(y = TX_NEW), fill = "#D9AF6B", 
            position = position_nudge(x = -0.1), width = 0.5) +
   si_style_ygrid() +
   scale_y_continuous(labels = comma) +
   labs(x = NULL, y = NULL) +
   expand_limits(x = c(0, 9)) 

 # Linkage plot - #b08472
 top_hts <- df_linkage %>% 
   ggplot(aes(x = period, group = 1)) +
   geom_line(aes(y = linkage), color = grey50k, size = 0.5) +
   geom_point(aes(y = linkage), shape = 19, color = "#b08472",  size = 3) + 
   geom_point(aes(y = linkage), shape = 1, color = grey90k,  size = 3) + 
   geom_text(aes(y = linkage, label = percent(linkage, 1)), 
             size = 9/.pt,
             family = "Source Sans Pro",
             fontface = "bold", 
             color = "#b08472", 
             vjust = -1.5) +
  si_style_nolines() +
   expand_limits(y = c(.85, 1), x = c(0, 9)) +
 theme(axis.text.y = element_blank(), 
       axis.text.x = element_blank()) +
   labs(x = NULL, y = NULL) +
   annotate("text", x = 8.5, y = 0.87, label = "Linkage", 
            size = 11/.pt, color = "#b08472")

 top_hts / bottom_hts +
   plot_layout(heights = c(1, 4))
 si_save("Images/Linkage_summary.png")
 

# POSITIVITY --------------------------------------------------------------

 
 # POSITIVITY (TARGETS / RESULTS)
 df_hts_combo <- 
   df_linkage %>% 
   mutate(fy = substr(period, 3, 4)) %>% 
   group_by(fy) %>% 
   mutate(across(2:4, cumsum, .names = "{.col}_cmltv")) %>%
   ungroup() %>% 
   left_join(df_hts_tgt %>% rename_with(~str_c(., "_tgt"), .cols = where(is.double))) %>% 
   mutate(achv_hts = HTS_TST_cmltv / HTS_TST_tgt,
          achv_hts_pos = HTS_TST_POS_cmltv / HTS_TST_POS_tgt,
          achv_tx_new = TX_NEW_cmltv / TX_NEW_tgt)
 
 df_hts_combo %>% 
   ggplot(aes(x = period)) +
   geom_col(aes(y = HTS_TST), fill = "#e0d4db", width = 0.5,
            position = position_nudge(x = 0.1)) +
   geom_col(aes(y = HTS_TST_POS), fill = "#855C75", width = 0.5) +
   geom_text(aes(y = HTS_TST_POS, label = percent(HTS_TST_POS/HTS_TST, 1)),
             size = 11/.pt, 
             family = "Source Sans Pro", 
             color = grey90k,
             vjust = -0.5) +
   #geom_col(aes(y = HTS_TST_cmltv), width = 0.5, fill = grey50k) +
   #geom_col(aes(y = HTS_TST_POS_cmltv), width = 0.5, fill = "#855C75") +
   si_style_ygrid() +
   scale_y_continuous(labels = comma) +
   labs(x = NULL, y = NULL, title = "TESTING POSITIVITY TRENDS",
        caption = metadata$caption)+
   coord_cartesian(expand = F)
 si_save("Images/HTS_positivity_summary.png")

# TODO  ============================================================================
# REPEAT BY IPS
 
 
