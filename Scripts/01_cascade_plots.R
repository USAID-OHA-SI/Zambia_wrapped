# PROJECT: Create Visuals for FY22 Q4 Data Review
# PURPOSE: Munge and Analysis of Zambia SI Data
# AUTHOR: Tim Essam | SI
# REF ID:   98614ee3
# LICENSE: MIT
# DATE: 2022-10-12
# NOTES: Tim Essam | SI

# LOCALS & SETUP ============================================================================

  # Install dev version of cascade and gophr
  remotes::install_github("USAID-OHA-SI/gophr", "09ada4c")
  remotes::install_github("USAID-OHA-SI/cascade", ref = "dev")

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

# LOAD DATA ============================================================================  

  df_msd <- read_msd(file_path) %>% 
    fix_mech_names() %>% 
    mutate(snu1 = str_remove_all(snu1, " Province"))
    
  plhiv <- read_msd(plhiv_path) %>% 
    filter(operatingunit == "Zambia") %>% 
    mutate(snu1 = str_remove_all(snu1, " Province"))
  
  plhiv_num <- plhiv %>% 
    filter(indicator %in% c("PLHIV"),  
           fiscal_year == (metadata$curr_fy), 
           standardizeddisaggregate == "Total Numerator") %>% 
    group_by(fiscal_year, indicator ) %>% 
    summarize(plhiv = sum(targets, na.rm = T)) %>% 
    pull(plhiv)
    
  plhiv %>% count(fiscal_year, indicator) %>% spread(fiscal_year, n)

# MUNGE ============================================================================
  
  # All of PEPFAR Zambia cascade
  return_cascade(df_msd, 1)
  return_cascade(df_msd %>% filter(funding_agency == "USAID"), 1)
 
  # Generate plots for all agencies
  batch_cascade_plot(df_msd, imgpath = "Images/Cascade/All")
    
  # Generate plots for just USAID
  batch_cascade_plot(df_msd %>% filter(funding_agency == "USAID"),
                     imgpath = "Images/Cascade/USAID")
  
  # Loop over mechs
  batch_cascade_plot(df_msd %>% filter(mech_name == "DISCOVER"),
                     imgpath = "Images/Cascade/DISCOVER")
  
  batch_cascade_plot(df_msd %>% filter(mech_name == "SAFE"),
                     imgpath = "Images/Cascade/SAFE")
  
  batch_cascade_plot(df_msd %>% filter(mech_name == "ACTION HIV"),
                     imgpath = "Images/Cascade/ACTION_HIV")

  
# TREATMENT COVERAGE BY PROVINCE ------------------------------------------

  tx_curr_subnat <- 
    plhiv %>% filter(fiscal_year == metadata$curr_fy, 
                   indicator == "TX_CURR_SUBNAT", 
                   standardizeddisaggregate == "Age/Sex/HIVStatus") %>% 
    group_by(snu1, fiscal_year) %>% 
    summarise(tx_subnat = sum(targets, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(total = sum(tx_subnat))
  
  tx_curr <- df_msd %>% 
    filter(funding_agency == "USAID", 
           fiscal_year == metadata$curr_fy, 
           indicator == "TX_CURR", 
           standardizeddisaggregate == "Total Numerator") %>% 
    group_by(snu1, fiscal_year) %>% 
    summarise(across(matches("targ|cumulative"), sum, na.rm = T)) %>% 
    ungroup()
  
   tx_usaid <- 
    tx_curr %>% 
    janitor::adorn_totals("row",,,, -fiscal_year) %>% 
    as.data.frame() %>% 
    mutate(snu1 = ifelse(snu1 == "Total", "USAID", snu1),
           fiscal_year = 2022) %>% 
    mutate(achv = cumulative / targets)
   
   aid_tx_achv <- tx_usaid %>% 
     filter(snu1 == "USAID") %>% 
     pull(achv)
   
   aid_tx_tot <- tx_usaid %>% 
     filter(snu1 == "USAID") %>% 
     pull(cumulative)
  
  # Merge'em for plotting
  tx_snu <- tx_curr_subnat %>% 
    left_join(tx_curr) %>% 
    mutate(achv = cumulative/targets, 
           cov = cumulative / tx_subnat,
           snu1_order = fct_reorder(snu1, tx_subnat, .desc = T)) %>% 
    arrange(snu1_order)
  
  tx_snu %>% 
    filter(snu1 %ni% c("Lusaka", "Southern", "Western", "Eastern")) %>% 
    ggplot(aes(x = snu1_order)) +
    geom_col(aes(y = tx_subnat), fill = grey10k, width = 0.5, alpha = 0.5) +
    geom_errorbar(aes(ymax = tx_subnat, ymin = tx_subnat), width = 0.5,
                  size = 0.5, color = grey50k) +
    geom_col(aes(y = targets), fill = scooter_med, width = 0.5, 
             position = position_nudge(x = 0.1)) +
    geom_col(aes(y = cumulative), fill = scooter, width = 0.5, 
              position = position_nudge(x = 0.2)) +
    geom_label(aes(y = cumulative, label = percent(achv, 1)), 
                   size = 10/.pt,
                   family = "Source Sans Pro", 
               position = position_nudge(x = 0.2, y = -5000)) +
    annotate("text", x = 1.65, y = 245000, label = "TX_CURR_SUBNAT Targets", 
             size = 8/.pt, family = "Source Sans Pro") +
    scale_y_continuous(labels = comma) +
    labs(x = NULL, y = NULL, 
         title = glue("USAID TX_CURR WAS AT {comma(aid_tx_tot)} IN {metadata$curr_pd}") ,
         caption = metadata$caption) +
    si_style_ygrid()
  
  tx_snu %>% 
    filter(snu1 %ni% c("Lusaka", "Southern", "Western", "Eastern")) %>% 
    mutate(snu1_order = fct_reorder(snu1, achv, .desc = T)) %>% 
    ggplot(aes(x = snu1_order)) +
    geom_col(aes(y = 1), fill = grey20k, width = 0.5) +
    geom_col(aes(y = achv), fill = scooter_med, width = 0.5) +
    geom_hline(yintercept = 1, color = grey50k, linetype = "dotted") +
    geom_hline(yintercept = aid_tx_achv, color = scooter) +
    annotate("text", x = 6.5, y = aid_tx_achv + 0.02, 
             label = glue("{percent(aid_tx_achv)}\n USAID achievement"), 
             color = scooter,
             family = "Source Sans Pro")  +
    expand_limits(x = c(1, 7)) +
    si_style_ygrid() +
    scale_y_continuous(labels = percent) +
    labs(x = NULL, y = NULL, 
         title = glue("USAID TX_CURR ACHIEVEMENT WAS AT {percent(aid_tx_achv)} IN {metadata$curr_pd}"),
         caption = metadata$caption)
  

# VIRAL LOAD & COVERAGE ---------------------------------------------------

  df_vl <- df_msd %>% 
    filter(funding_agency == "USAID",
           indicator %in% c("TX_CURR","TX_CURR_Lag2", "TX_PVLS"),
           standardizeddisaggregate %in% c("Age/Sex/HIVStatus", 
                                           "Age/Sex/Indication/HIVStatus")
           ) %>% 
    clean_indicator() %>% 
    group_by(indicator, fiscal_year) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd(include_type = FALSE) %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}") %>% 
    mutate(vlc = tx_pvls_d/tx_curr_lag2,
           vls = tx_pvls / tx_pvls_d,
           vls_adj = tx_pvls /tx_curr_lag2) 
  
  
  
  top <- df_vl %>% 
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
    annotate("text", x = 11.5, y = .97, label = "Viral Load Suppression",
             color = burnt_sienna, size = 10/.pt,
             hjust = 0.1) +
    annotate("text", x = 11.5, y = .69, label = "Viral Load Coverage",
             color = denim, size = 10/.pt,
             hjust = 0.1) +
    si_style_nolines() +
    expand_limits(x = c(1, 14), y = c(0.7,1.05)) +
    scale_x_discrete(position = "top") +
    theme(axis.text.y = element_blank(), 
          axis.text.x = element_blank()) +
    labs(x = NULL, y = NULL)

  
  bottom <- df_vl %>% 
    ggplot(aes(x = period)) +
    geom_col(aes(y = tx_curr_lag2), fill = grey10k) +
    geom_col(aes(y = tx_pvls_d), fill = denim) +
    si_style_ygrid() +
    scale_y_continuous(labels = comma) +
    expand_limits(x = c(1, 14)) +
    labs(x = NULL, y = NULL) +
    annotate("segment", x = 11.5, xend = 11.5, y = 355000, yend = 510000, 
             color = grey70k) +
    annotate("text", x = 11.65, y = 450000, label = "Coverage gap", 
             hjust = 0, size = 11/.pt, family = "Source Sans Pro", 
             color = grey70k)+
    annotate("text", x = 11, y = 525000, label = "TX_CURR_LAG2", 
             size = 8/.pt, family = "Source Sans Pro", color = grey50k)
  
  top / bottom + plot_layout(heights = c(1, 3)) +
    plot_annotation(title = glue("VIRAL LOAD SUMMARY FOR {metadata$curr_fy}"),
                    caption = metadata$caption)
    
  

# SPINDOWN ============================================================================

