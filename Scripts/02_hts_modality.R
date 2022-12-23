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
  library(rcartocolor)
  
  
  # SI specific paths/functions  
  load_secrets()
  merdata <- file.path(glamr::si_path("path_msd"))
  file_path <- return_latest(folderpath = merdata,
                             pattern = "PSNU_IM.+Zambia")
  
  # Refid
  refid <- "0bb2a8eb"
  
  # Grab metadata
  get_metadata(file_path)
  
  # Functions  
  source("Scripts/99_custom_functions.r")
  

# LOAD DATA ============================================================================  

  df_msd <- read_msd(file_path) %>% 
    fix_mech_names() %>% 
    swap_targets() %>% 
    filter(funding_agency == "USAID")


# HTS MODALITY ------------------------------------------------------------

  # To plot USAID, reset mech_code to a constant
    munge_modality(df_msd %>% mutate(mech_code = 123456, mech_name = "USAID"), 
                 mech_code == 123456) %>% 
    plot_modality(.)
    si_save("Graphics/Index_testing_summary_by_modality.svg")

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
 si_save("Graphics/Linkage_summary.svg")
 

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
 si_save("Graphics/HTS_positivity_summary.svg")

# INDEX vs OTHER FOR PEDS  ============================================================================

 
 df_msd_tst <- read_msd(file_path) %>% 
   fix_mech_names() %>% 
   swap_targets() %>% 
   filter(str_detect(indicator, "HTS"),
          trendscoarse == "<15")
 
 munge_modality_index <- function(df, ...){   
   df_hts_full <- df %>% 
     filter(indicator == "HTS_TST_POS",
            standardizeddisaggregate == "Modality/Age/Sex/Result",
            fiscal_year <= metadata$curr_fy) %>% 
     mutate(mod_type = case_when(
       str_detect(modality, "Index") ~ "Index",
       TRUE ~ "Other")
     ) %>%
     group_by(fiscal_year, mod_type, ...) %>%
     summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>%
     ungroup() %>%
     reshape_msd() %>%
     select(-period_type) %>%
     group_by(period, ...) %>%
     mutate(contribution = value/sum(value)) %>%
     ungroup() %>%
     mutate(start = case_when(period == min(period) ~ contribution),
            end = case_when(period == max(period) ~ contribution)) %>%
     mutate(mod_order = fct_reorder(mod_type, value, .desc = T)) %>% 
     complete(mod_type, period, ...) %>% 
     group_by(mod_type, ...) %>% 
     fill(mod_order, .direction = "up") %>% 
     group_by(period, ...) %>% 
     mutate(pd_tot = sum(value, na.rm = T), 
            pd_25 = pd_tot * 0.25, 
            pd_50 = pd_tot * 0.5,
            pd_75 = pd_tot * 0.75) %>% 
     ungroup() %>% 
     mutate(mod_color = case_when(
       mod_type == "Index" ~ "#855C75", 
       TRUE ~ "#D9AF6B"
     ),
     note = case_when(
       mod_type == "Index" & period == "FY20Q1" ~ "HTS_TST_POS",
       TRUE ~ NA_character_
     )) %>% 
     filter(!is.na(mod_order))
   return(df_hts_full)
 }

 df_tst_snu <- munge_modality_index(df_msd_tst, snu1) 
 df_tst <- munge_modality_index(df_msd_tst)

 
 # PLOT results for OU and the SNU
 df_tst %>% 
   filter(mod_type == "Index") %>% 
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
   geom_text(aes(y = value, label = percent(start, 1)), size = 7/.pt, vjust = -0.5) +
   geom_text(aes(y = value, label = percent(end, 1)), size = 7/.pt,  vjust = -0.5) +
   labs(x = NULL, y = NULL,
        title = glue("PEDIATRIC HTS INDEX TESTING COMPARED TO ALL OTHER FORMS FOR ALL OF ZAMBIA"),
        caption = glue("{metadata$caption}")) +
   theme(legend.position = "none") +
   scale_y_continuous(label = comma) +
   scale_x_discrete(labels = c("FY20Q1", "", "", "",
                               "FY21Q1", "", "", "",
                               "FY22Q1", "", "", "")) +
   si_style_ygrid()  
 
 si_save("Graphics/HTS_index_mod_peds.svg", scale = 1.33)
 
 df_tst_snu %>% 
   mutate(snu1 = str_remove_all(snu1, " Province")) %>% 
   filter(str_detect(snu1, "Military", negate = T),
          mod_type == "Index") %>% 
   mutate(ymax = case_when(
            snu1 %in% c("Lusaka", "Copperbelt", "Southern", "Central", "Western") ~ 684,
            TRUE ~ 200),
          snu_order = fct_reorder(snu1, pd_tot, .desc = T)
          ) %>% 
   ggplot(aes(x = period)) +
   geom_blank(aes(ymax = ymax)) +
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
   facet_wrap(~snu_order, scales = "free_y", nrow = 2) +
   geom_text(aes(y = value, label = percent(start, 1)), size = 7/.pt, vjust = -0.5) +
   geom_text(aes(y = value, label = percent(end, 1)), size = 7/.pt,  vjust = -0.5) +
   labs(x = NULL, y = NULL,
        title = glue("PEDIATRIC HTS INDEX TESTING TRENDS COMPARED TO ALL OTHER FORMS BY PROVINCE"),
        caption = glue("{metadata$caption}")) +
   theme(legend.position = "none") +
   scale_y_continuous(label = comma) +
   scale_x_discrete(labels = c("FY20Q1", "", "", "",
                               "FY21Q1", "", "", "",
                               "FY22Q1", "", "", "")) +
   si_style_ygrid(facet_space = 0.75)  
 
 si_save("Graphics/HTS_index_mod_peds_snu.svg", scale = 1.33)


# TESTING TRENDS FOR PEDS OVERALL -----------------------------------------

 df_tst_trends <- df_msd_tst %>% 
   filter(indicator %in% c("HTS_TST_POS", "HTS_TST"),
          standardizeddisaggregate == "Modality/Age/Sex/Result",
          fiscal_year <= metadata$curr_fy,
          trendscoarse == "<15") %>% 
   group_by(fiscal_year, indicator) %>%
   summarise(across(matches("qtr"), sum, na.rm = TRUE)) %>%
   ungroup() %>%
   reshape_msd(direction = "quarters") %>% 
   select(-results_cumulative) %>% 
   pivot_wider(names_from = "indicator", 
               values_from = "results") %>% 
   mutate(positivity = HTS_TST_POS/HTS_TST) %>% 
   pivot_longer(cols = HTS_TST:HTS_TST_POS,
                names_to = "indicator",
                values_to = "values") %>% 
   mutate(fill_color = ifelse(indicator == "HTS_TST_POS", "#855C75", "#526a83"))
 
 top <- df_tst_trends %>% 
   ggplot(aes(x = period, fill = fill_color)) +
   geom_col(aes(y = values), width = 0.75) +
   facet_wrap(~indicator, nrow = 2, scales = "free_y") +
   scale_fill_identity() +
   scale_y_continuous(labels = label_number_si()) +
   si_style_ygrid() +
   labs(x = NULL, y = NULL, 
        title = "PEDIATRIC TESTING TRENDS FOR ALL OF ZAMBIA")

 bottom <- df_tst_trends %>% 
   filter(indicator == "HTS_TST") %>% 
   ggplot(aes(x = period, y = positivity, group = 1)) +
   geom_area(fill = "#Af6458", alpha = 0.25) +
   geom_line(color = "#FFFFFF", size = 2) +
   geom_line(color = "#Af6458", size = 1) +
   geom_point(data = . %>% filter(period == min(period)), size = 3) +
   geom_point(data = . %>% filter(period == max(period)), size = 3) +
   geom_text(data = . %>% filter(period == min(period)), aes(label = percent(positivity, 0.01))) +
   geom_text(data = . %>% filter(period == max(period)), aes(label = percent(positivity, 0.01))) +
   scale_y_continuous(labels = percent, limits = c(0, 0.04)) +
   facet_wrap(~ "POSITIVITY") +
   si_style_ygrid() +
   labs(x = NULL, y = NULL) +
   theme(axis.text.x = element_blank())
 
 top / bottom + plot_layout(heights = c(4, 2)) +
   plot_annotation(caption = glue("{metadata$caption}"))
 
 si_save("Graphics/HTS_peds_trends.svg", scale = 1.33)
 
                   