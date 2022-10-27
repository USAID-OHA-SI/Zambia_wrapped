# PROJECT: VLS and VLC
# PURPOSE: Munge and Analysis of 3rd 95
# AUTHOR: Tim Essam | SI
# REF ID:   77b30310
# LICENSE: MIT
# DATE: 2022-10-26
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
    library(fontawesome)
    library(lubridate)


  # SI specific paths/functions  
    load_secrets()
    merdata <- file.path(glamr::si_path("path_msd"))
    file_path <- return_latest(folderpath = merdata,
                             pattern = "Genie-PSNUByIMs-Zambia")
  
  # REF ID for plots
    ref_id <- "77b30310"
    
  # Grab metadata
    get_metadata(file_path) 
    source("Scripts/99_custom_functions.R")

# LOAD DATA ============================================================================  

  df_site <- read_msd(file_path) %>% filter(funding_agency == "USAID")

# MUNGE ============================================================================
    
    df_vl <- df_site %>% 
      filter(funding_agency == "USAID",
             indicator %in% c("TX_CURR","TX_CURR_Lag2", "TX_PVLS"),
             standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age/Sex/Indication/HIVStatus"),
             ageasentered != "Unknown Age") %>% 
      mutate(ageasentered = case_when(trendscoarse == "<15" ~ trendscoarse,
                                      ageasentered %in% c("50-54","55-59", "60-64", "65+") ~ "50+",
                                      TRUE ~ ageasentered)) %>% 
      clean_indicator() %>% 
      group_by(snu1, indicator, ageasentered, fiscal_year) %>% 
      summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
      reshape_msd(include_type = FALSE) %>% 
      pivot_wider(names_from = indicator,
                  names_glue = "{tolower(indicator)}")
    
    df_vl <- df_vl %>% 
      arrange(snu1, ageasentered, period) %>% 
      group_by(snu1, ageasentered) %>% 
      mutate(vlc = tx_pvls_d/tx_curr_lag2,
             vls = tx_pvls / tx_pvls_d,
             vls_adj = tx_pvls /tx_curr_lag2) %>% 
      ungroup() %>% 
      filter(period == max(period))
    
    df_avg <- df_vl %>% 
      group_by(snu1) %>% 
      summarise(avg_vlc = mean(vlc),
                avg_vls = mean(vls),
                avg_vls_adj = mean(vls_adj)) %>% 
      ungroup()
    
    
    df_usaid_vl <- df_vl %>% 
      summarise(vlc = sum(tx_pvls_d, na.rm = TRUE)/sum(tx_curr_lag2, na.rm = TRUE),
                vls = sum(tx_pvls, na.rm = TRUE) / sum(tx_pvls_d, na.rm = TRUE),
                vls_adj = sum(tx_pvls, na.rm = TRUE) /sum(tx_curr_lag2, na.rm = TRUE))
    
    
    df_vl %>% 
      mutate(tx_curr_pct = 1) %>% 
      ggplot(aes(tx_curr_pct, ageasentered)) +
      geom_vline(data = df_avg, aes(xintercept = avg_vlc, color = grey80k)) +
      geom_col(fill = grey10k, alpha = 0.6) +
      geom_col(aes(vls), fill = scooter_light, alpha = 0.75, width = 0.75) +
      geom_col(aes(vlc), fill = scooter, alpha = 1, width = 0.75) +
      geom_vline(xintercept = 1, size = 0.5, color = grey30k) +
      geom_vline(xintercept = 0, size = 0.5, color = grey30k) +
      geom_richtext(aes(vls, label = glue("<span style='color:#505050'>{percent(vls, 1)}</span>")),
                    label.color = NA, fill = NA,
                    nudge_x = -0.04, size = 3,
                    family = "Source Sans Pro") +
      geom_richtext(aes(vlc, label = glue("<span style='color:white'>{percent(vlc, 1)}</span>")),
                    label.color = NA, fill = NA,
                    nudge_x = -0.04, size = 3,
                    family = "Source Sans Pro") +
      geom_text(data = df_avg, aes(x = avg_vlc, y = avg_vlc, label = percent(avg_vlc, 1)),
                hjust = .3, vjust = 1.6,
                family = "Source Sans Pro",
                color = grey90k, size = 10/.pt) +
      facet_wrap(~snu1) +
      coord_cartesian(expand = T, clip = "off") +
      scale_color_identity() +
      scale_x_continuous(label = percent) +
      labs(x = NULL, y = NULL, 
           title = glue("In {metadata$curr_pd}, USAID VLC and VLS are at {percent(df_usaid_vl$vlc, 1)} and {percent(df_usaid_vl$vls, 1)} respectively") %>% toupper,
           subtitle = glue("<span style='color:{scooter}'>**VLC**</span> and <span style='color:{scooter_light}'>**VLS**</span> rates"),
           caption = glue("Source: {metadata$source}")) +
      si_style_nolines() +
      theme(panel.spacing = unit(.5, "line"),
            plot.subtitle = element_markdown(),
            strip.text = element_markdown(),
            axis.text.x = element_blank()) 
    
    si_save("Graphics/VLC_VLS_by_province.svg")
 