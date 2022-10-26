# PROJECT: FY22 Q4 POART'ish review
# PURPOSE: Munge and Analysis of HTS data / modalities
# AUTHOR: Tim Essam | SI
# REF ID:   0bb2a8eb
# LICENSE: MIT
# DATE: 2022-10-14
# NOTES: Tim Essam | SI

# LOCALS & SETUP =========================================================================
    
  # ASSUMPTIONS
  # 99 and 01 scripts have been loaded
  

# LOAD DATA ============================================================================  

   df_hts <- 
     df_msd %>% 
      filter(indicator == "HTS_TST_POS",
             standardizeddisaggregate == "Modality/Age/Sex/Result",
             fiscal_year <= metadata$curr_fy, 
             funding_agency == "USAID"
      ) %>% 
      mutate(mod_type = case_when(
        str_detect(modality, "Index") ~ "Index",
        str_detect(modality, "OtherPITC") ~ "Other PITC",
        str_detect(modality, "PMTCT") ~ "PMTCT",
        modality == "VCT" ~ "VCT",
        str_detect(modality, "SNSMod") ~ "Community SNS",
        TRUE ~ "Other")
      ) %>% 
      group_by(fiscal_year, mod_type) %>%
      summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
      ungroup() %>% 
      reshape_msd() %>% 
      select(-period_type) %>% 
      group_by(period) %>% 
      mutate(contribution = value/sum(value)) %>% 
      ungroup() %>% 
      mutate(start = case_when(period == min(period) ~ contribution),
             end = case_when(period == max(period) ~ contribution)) %>% 
      mutate(mod_order = fct_reorder(mod_type, value, .desc = T)) 
   
   # Expand so Community SNS is available for all periods
   # Add in desired colors for bar fills
   # rcartocolor::carto_pal(6, "Antique)
   
   df_hts_full <- 
     df_hts %>% 
     complete(mod_type, period) %>% 
     group_by(mod_type) %>% 
     fill(mod_order, .direction = "up") %>% 
     group_by(period) %>% 
     mutate(pd_tot = sum(value, na.rm = T), 
            pd_25 = pd_tot * 0.25, 
            pd_50 = pd_tot * 0.5,
            pd_75 = pd_tot * 0.75) %>% 
     ungroup() %>% 
     mutate(mod_color = case_when(
       mod_type == "Index" ~ "#855C75", 
       mod_type == "VCT" ~ "#D9AF6B",
       mod_type == "Other PITC" ~ "#AF6458",
       mod_type == "PMTCT"~ "#736F4C",
       mod_type == "Community SNS" ~ "#526A83",
       TRUE ~ "#7C7C7C"
     ),
     note = case_when(
       mod_type == "Index" & period == "FY20Q1" ~ "HTS_TST_POS",
       TRUE ~ NA_character_
     ))
   
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

  df_hts_full %>% 
      ggplot(aes(x = period)) +
     geom_col(aes(y = pd_tot), fill = grey20k) +
     geom_col(aes(y = value, fill = mod_color)) +
    geom_errorbar(aes(ymin = pd_25, ymax = pd_25), 
                  size = 0.25, color = "white", 
                  linetype = "dotted") +
    geom_errorbar(aes(ymin = pd_50, ymax = pd_50), 
                  size = 0.25, color = "white", 
                  linetype = "dotted") +
    geom_errorbar(aes(ymin = pd_75, ymax = pd_75), 
                  size = 0.25, color = "white", 
                  linetype = "dotted") +
    scale_fill_identity() +
    facet_wrap(~mod_order) +
    geom_text(aes(y = value, label = percent(start, 1)), size = 7/.pt, vjust = -0.5) +
    geom_text(aes(y = value, label = percent(end, 1)), size = 7/.pt,  vjust = -0.5) +
    geom_text(aes(y = pd_tot, label = note), size = 8/.pt, color = "#7C7C7C",
              hjust = 0.2, vjust = -0.25) +
    labs(x = NULL, y = NULL,
         title = glue(""),
         caption = glue("Source: {metadata$caption}")) +
    theme(legend.position = "none") +
    scale_y_continuous(label = comma) +
    scale_x_discrete(labels = c("FY21Q1", "", "", "",
                                "FY22Q1", "", "", "")) +
    si_style_ygrid(facet_space = 0.5) 
 si_save("Images/Index_testing_summary_by_modality")
  

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
 
 
