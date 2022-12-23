# PROJECT: AMARA's POART DREAMS
# PURPOSE: Munge and Analysis of
# AUTHOR: Tim Essam | SI
# REF ID:   820e4a95
# LICENSE: MIT
# DATE: 2022-11-16
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
    library(extrafont)
    
    
  # SI specific paths/functions  
    load_secrets()
    merdata <- file.path(glamr::si_path("path_msd"))
    file_path <- return_latest(folderpath = merdata,
        pattern = "PSNU_IM_FY20-23_20221114_v1_1_Zambia.zip")
      
    plhiv_path <- return_latest(folderpath = merdata,
                                pattern = "SUBNAT")
    
  # Grab metadata
    get_metadata(file_path)
  
  # REF ID for plots
    ref_id <- "820e4a95"
    
  # Functions  
  source("Scripts/99_custom_functions.R")
    
    add_plhiv_est <- function(df, df2, ...){
      df %>% 
        group_by(fiscal_year, indicator, ...) %>% 
        summarize(across(matches("targ|qtr"), sum,  na.rm = T)) %>%
        ungroup() %>% 
        mutate(across(starts_with("qtr"), ~ifelse(. == 0, -999, .))) %>% 
        reshape_msd(direction = "quarters") %>%
        select(-results_cumulative) %>%
        mutate(snu1 = str_remove_all(snu1, " Province"),
               results = ifelse(results == -999, NA_real_, results)) %>%
        left_join(df2, by = c("fiscal_year", 'snu1')) %>% 
        mutate(gap_results = PLHIV - results, 
               gap_cov = results/PLHIV)
    }

# LOAD DATA ============================================================================  

  df_msd <- read_msd(file_path) 
  
  plhiv <- read_msd(plhiv_path) %>% 
    filter(operatingunit == "Zambia") %>% 
    mutate(snu1 = str_remove_all(snu1, " Province"))
  
  plhiv_ayp <- plhiv %>% 
    filter(indicator %in% c("PLHIV", "TX_CURR_SUBNAT"),  
           standardizeddisaggregate == "Age/Sex/HIVStatus",
           ageasentered %in% c("15-19", "20-24")) %>% 
    group_by(fiscal_year, indicator, snu1) %>% 
    summarize(value = sum(targets, na.rm = T)) %>% 
    ungroup() %>% 
    spread(indicator, value) %>% 
    arrange(snu1)
  
  plhiv_peds <- plhiv %>% 
    filter(indicator %in% c("PLHIV", "TX_CURR_SUBNAT"),  
           standardizeddisaggregate == "Age/Sex/HIVStatus",
           trendscoarse == "<15") %>% 
    group_by(fiscal_year, indicator) %>% 
    summarize(value = sum(targets, na.rm = T)) %>% 
    ungroup() %>% 
    spread(indicator, value) %>% 
    arrange(snu1)
  
  plhiv %>%
    count(fiscal_year, indicator, standardizeddisaggregate) %>% spread(fiscal_year, n) %>% prinf()

# MUNGE ============================================================================
  
  #  Munge VLC by 15-19, 20-24 M/F
  df_vlc <- 
      df_msd %>% 
      filter(ageasentered %in% c("15-19", "20-24")) %>% 
      create_vl_df(ageasentered, sex) %>% 
      pivot_longer(cols = c(vlc, vls), 
                   names_to = "VL", 
                   values_to = "value") %>% 
      mutate(age_sex = str_c(ageasentered, sex, sep = " "))
  
  
  # Munge TX_CURR w/ PLHIV to show "gaps" for AYP across last four quarters by province
  df_ayp_tx <- 
    df_msd %>% 
    filter(ageasentered %in% c("15-19", "20-24"),
           standardizeddisaggregate == "Age/Sex/HIVStatus", 
           indicator == "TX_CURR") %>% 
    add_plhiv_est(., plhiv_ayp, snu1)
  
  df_peds_tx <- 
    df_msd %>% 
    filter(trendscoarse == "<15",
           standardizeddisaggregate == "Age/Sex/HIVStatus", 
           indicator == "TX_CURR") %>% 
    add_plhiv_est(., plhiv_peds, snu1)
  
  # %>% 
  #   arrange(snu1) %>% 
  #   filter(snu1 == "Lusaka")

  # TX_CURR and TX_NN for AYP
  df_ayp_tnn <- df_msd %>% 
    filter(ageasentered %in% c("15-19", "20-24"),
           standardizeddisaggregate == "Age/Sex/HIVStatus", 
           indicator %in% c("TX_CURR", "TX_NET_NEW")) %>% 
      group_by(fiscal_year, indicator) %>% 
      summarize(across(matches("qtr"), sum,  na.rm = T)) %>% 
    reshape_msd() %>% 
    mutate(value2 = abs(value))
  
# VIZ ============================================================================

  #  Create VIZ with two facets, 1 summarizing VLC one summarizing VLS
    df_vlc %>% 
      mutate(labels = case_when(
        period == metadata$curr_pd & age_sex == "15-19 Female" & VL == "vls" ~ "Viral Load\nSuppression",
        period == metadata$curr_pd & age_sex == "15-19 Female" & VL == "vlc" ~ "Viral Load\nCoverage",
        TRUE ~ NA_character_
      )) %>% 
      ggplot(aes(x = period, y = value, group = VL, color = VL, fill = VL)) +
      geom_line(size = 1) +
      geom_point(shape = 21, size = 3, color = "white", stroke = 0.25) +
      geom_text(aes(label = percent(value, 1)), size = 9/.pt,
                family = "Source Sans Pro", 
                vjust = -1) +
      geom_text(aes(label = labels), size = 9/.pt,
                family = "Source Sans Pro", 
                hjust = -0.2) +
    facet_wrap(~age_sex) +
      si_style_ygrid() +
      scale_y_continuous(labels = percent) +
      scale_color_manual(values = c("vlc" = denim, "vls" = burnt_sienna)) +
      scale_fill_manual(values = c("vlc" = denim, "vls" = burnt_sienna)) +
      scale_x_discrete(breaks = every_nth(n = 2)) +
      labs(x = NULL, y = NULL, caption = glue("{metadata$caption}"),
           title = "VIRAL LOAD COVERAGE INCREASED STEADILY ACROSS ALL AYP SEX/AGE BANDS IN FY22") +
      expand_limits(x = c(1, 10), y = c(0.6,1.0)) +
      theme(legend.position = "none")
    
    si_save("Graphics/AYP_VL_trends.svg", scale = 1.25)


# TX_CURR_NN SUMMARY ------------------------------------------------------

    library(ggforce)
  top <-  df_ayp_tnn %>% 
      mutate(indic = fct_relevel(indicator, c("TX_NET_NEW", "TX_CURR")),
             indic_color = case_when(
               indicator == "TX_CURR" ~ scooter_med, 
               indicator == "TX_NET_NEW" & value > 0 ~ scooter, 
               TRUE ~ old_rose
              )
             ) %>% 
      ggplot(aes(x = period, group = indic)) +
      geom_col(aes(y = value2, fill = indic_color), width = 0.5) +
      geom_text(aes(y = value2, label = comma(value)), 
                position = position_stack(vjust = 1)) +
      scale_fill_identity() +
      coord_cartesian(ylim=c(85000,100000), expand = F) +
    labs(x = NULL, y = NULL, title = "TX_CURR & TX_NET_NEW TRENDS, ALL AGENCIES") +
    si_style_ygrid() +
    theme(axis.text.x = element_blank())

  bottom <-  df_ayp_tnn %>% 
    mutate(indic = fct_relevel(indicator, c("TX_NET_NEW", "TX_CURR")),
           indic_color = case_when(
             indicator == "TX_CURR" ~ scooter_med, 
             indicator == "TX_NET_NEW" & value > 0 ~ scooter, 
             TRUE ~ old_rose
           )
    ) %>% 
    ggplot(aes(x = period, group = indic)) +
    geom_col(aes(y = value2, fill = indic_color), width = 0.5) +
    scale_fill_identity() +
    scale_y_continuous(labels = label_number_si()) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 85000, ymax = 100000, 
             alpha = 0.1, color = grey50k) +
    si_style_ygrid() +
    coord_cartesian(expand = F) +
    labs(x = NULL, y= NULL, caption = glue("{metadata$caption}"))
  
  top / bottom +
    plot_layout(heights = c(3, 1))
  
    si_save("Graphics/TX_CURR_TX_NN_TRENDS.svg", scale = 1.25)
    
# VIZ TX_CURR COVERAGE GAP ------------------------------------------------

        
# VIZ TX_CURR GAP -- use geom_ribbon to capture high/low of gap
    df_ayp_tx %>% 
      filter(str_detect(snu1, "Military", negate = T))%>% 
      mutate(snu1 = fct_reorder(snu1, targets, .desc = T),
             ymax = case_when(
               snu1 %in% c("Copperbelt", "Lusaka", "Southern") ~ 40000, 
               snu1 %in% c("Luapula", "Muchinga", "NorthWestern") ~ 10000, 
               TRUE ~ 20000),
             ymin = ifelse(snu1 %in% c("Copperbelt", "Lusaka", "Southern"), 10000, 0)) %>%
      mutate(cumulative = results, 
             last_pd = ifelse(period == metadata$curr_pd, 1, 0)) %>% 
      adorn_achievement() %>% 
      ggplot(aes(x = period, group = snu1)) +
      # geom_blank(aes(y = 0)) +
      # geom_blank(aes(y = ymax)) +
      geom_line(aes(y = PLHIV, x = period), size = 0.75, color = grey50k, ) +
      geom_ribbon(aes(ymin = results, ymax = PLHIV), fill = old_rose, alpha = 0.15) +
      # geom_ribbon(aes(xmin = period, xmax = period, ymin = targets, ymax = PLHIV), fill = old_rose, alpha = 0.15) +
      geom_line(aes(y = targets, x = period), size = 0.75,color = grey70k,  linetype = "dashed") +
      geom_line(aes(y = results), size = 0.75, color = scooter) +
      geom_point(data = . %>% filter(period %in% c("FY21Q1", "FY22Q4")), aes(y = results), size = 3, color = scooter) +
      geom_text(data = . %>% filter(period %in% c("FY21Q1", "FY22Q4")), aes(y = results, label = percent(achievement, 1)),
                size = 8/.pt, 
                family = "Source Sans Pro",
                vjust = 1.25) +
      geom_text(data = . %>% filter(last_pd == 1), aes(y = PLHIV - .25*PLHIV, label = label_number_si()(gap_results))) +
      geom_text(data = . %>% filter(last_pd == 1), aes(y = PLHIV - .15*PLHIV, label = percent(gap_cov))) +
      facet_wrap(~snu1, scales = "free_y", nrow = 2) +
      si_style_ygrid() +
      scale_y_continuous(labels = label_number_si()) +
      scale_x_discrete(breaks = c("FY21Q1", "FY22Q1", "FY22Q4", "FY23Q4"))
    
    si_save("Graphics/TX_CURR_AYP_trends.svg", scale = 1.5)


# PEDS PLOT ---------------------------------------------------------------

    df_peds_tx %>% 
      filter(str_detect(snu1, "Military", negate = T))%>% 
      mutate(snu1 = fct_reorder(snu1, targets, .desc = T),
             ymax = case_when(
               snu1 %in% c("Copperbelt", "Lusaka", "Southern") ~ 40000, 
               snu1 %in% c("Luapula", "Muchinga", "NorthWestern") ~ 10000, 
               TRUE ~ 20000),
             ymin = ifelse(snu1 %in% c("Copperbelt", "Lusaka", "Southern"), 10000, 0)) %>%
      mutate(cumulative = results, 
             last_pd = ifelse(period == metadata$curr_pd, 1, 0)) %>% 
      adorn_achievement() %>% 
      ggplot(aes(x = period, group = snu1)) +
      # geom_blank(aes(y = 0)) +
      # geom_blank(aes(y = ymax)) +
      geom_line(aes(y = PLHIV, x = period), size = 0.75, color = grey50k, ) +
      geom_ribbon(aes(ymin = results, ymax = PLHIV), fill = old_rose, alpha = 0.15) +
      # geom_ribbon(aes(xmin = period, xmax = period, ymin = targets, ymax = PLHIV), fill = old_rose, alpha = 0.15) +
      geom_line(aes(y = targets, x = period), size = 0.75,color = grey70k,  linetype = "dashed") +
      geom_line(aes(y = results), size = 0.75, color = scooter) +
      geom_point(data = . %>% filter(period %in% c("FY21Q1", "FY22Q4")), aes(y = results), size = 3, color = scooter) +
      geom_text(data = . %>% filter(period %in% c("FY21Q1", "FY22Q4")), aes(y = results, label = percent(achievement, 1)),
                size = 8/.pt, 
                family = "Source Sans Pro",
                vjust = 1.5) +
      geom_text(data = . %>% filter(last_pd == 1), aes(y = PLHIV - .34*PLHIV, label = paste0("(", label_number_si()(gap_results), ")")),
                size = 8/.pt, 
                family = "Source Sans Pro") +
      geom_text(data = . %>% filter(last_pd == 1), aes(y = PLHIV - .3*PLHIV, label = percent(gap_cov)),
                size = 8/.pt, 
                family = "Source Sans Pro") +
      facet_wrap(~snu1, scales = "free_y", nrow = 2) +
      labs(x = NULL, y = NULL, title = "PLACEHOLDER",
           caption = glue("{metadata$caption}")) +
      si_style_ygrid() +
      scale_y_continuous(labels = label_number_si()) +
      scale_x_discrete(breaks = c("FY21Q1", "FY22Q1", "FY22Q4", "FY23Q4"))  
    
    si_save("Graphics/TX_CURR_peds_trends.svg", scale = 1.5)
    

# PEDS HTS_TST_POS TARGETS ACROSS TIME ------------------------------------

  df_peds_tst <- df_msd %>% 
      filter(trendscoarse == "<15", indicator == "HTS_TST_POS",
             standardizeddisaggregate == "Modality/Age/Sex/Result") %>% 
      group_by(snu1, fiscal_year, indicator) %>% 
      summarise(across(c("targets", "cumulative"), sum, na.rm = T)) %>%
      ungroup() %>% 
      adorn_achievement() %>% 
      mutate(snu1 = str_remove_all(snu1, " Province"))
    
  df_peds_tst %>% 
    filter(str_detect(snu1, "Military", negate = T)) %>% 
    group_by(snu1) %>% 
    mutate(tot = sum(targets, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(snu1_order = fct_reorder(snu1, tot, .desc = T),
           ymax = case_when(
             snu1 %in% c("Copperbelt", "Lusaka", "Southern", "Central", "Luapula") ~ 2500, 
             snu1 %in% c("Northern", "Western", "Muchinga", "Eastern", "NorthWestern") ~ 1000
            )
           ) %>% 
    ggplot(aes(x = fiscal_year, group = snu1)) +
    geom_blank(aes(y = 0)) +
    geom_blank(aes(y = ymax)) +
    geom_ribbon(data = . %>% filter(fiscal_year %ni% c("2023")), 
                aes(ymin = 0, ymax = cumulative), fill = scooter_med, alpha = 0.15) +
    geom_line(aes(y = targets), linewidth = 0.75, color = grey70k,  linetype = "dashed") +
    geom_line(data = . %>% filter(fiscal_year %ni% c("2023")), aes(y = cumulative), linewidth = 0.75, color = scooter) +
    geom_point(data = . %>%  filter(fiscal_year %ni% c("2023")), aes(y = cumulative), size = 3, color = scooter) +
    geom_text(data = . %>% filter(fiscal_year %ni% c("2023")), aes(y = cumulative, label = percent(achievement, 1)), family = "Source Sans Pro", size = 8/.pt) +
    facet_wrap(~snu1_order, nrow = 2, scales = "free_y") +
    labs(x = NULL, y = NULL, title = "PLACEHOLDER",
         caption = glue("{metadata$caption}")) +
    si_style_ygrid() +
    scale_y_continuous(labels = label_number_si()) 
  
  si_save("Graphics/HTS_TST_POS_peds_trends.svg", scale = 1.5)
    
# TESTING MUNGE AND VIZ ============================================================================

  df_msd %>% 
      filter(indicator %in% c("HTS_TST", "HTS_TST_POS"),
             standardizeddisaggregate == "Modality/Age/Sex/Result",
             ageasentered %in% c("15-19", "20-24")) %>% 
      group_by(fiscal_year, indicator) %>% 
      summarize(across(matches("targ|qtr"), sum, na.rm = T)) %>% 
      reshape_msd(direction = "quarters") %>% 
      select(-c(targets, results_cumulative)) %>% 
      spread(indicator, results) %>% 
      mutate(pos = HTS_TST_POS/HTS_TST) %>% 
      ggplot(aes(x = HTS_TST, y = HTS_TST_POS)) +
      geom_path(lineend = "round", size = 1, color = grey50k) +
      geom_point(aes(size = pos, color = pos)) +
      geom_text(aes(label = percent(pos, 0.01)), size = 8/.pt, 
                family = "Source Sans Pro", 
                vjust = 2) +
      scale_color_si(palette = "scooters") +
      si_style() +
      scale_x_continuous(labels = label_number_si(), position = "top") +
      scale_y_continuous(labels = label_number_si(), position = "left") +
      theme(legend.position = "none") +
      labs(x = "Number of individuals who received HIV Testing Services", y = "Number of individuals receiving positive results", title = "CASE FINDING TRENDS FOR AYP FY21 - FY22",
           caption = glue("{metadata$caption}"))
      

    si_save("Graphics/AY_testing_trends_scatterpath.svg", scale = 1.25)


# INDEX TESTING SUMMARY ---------------------------------------------------

   df_ayp_modality <-  df_msd %>% 
      filter(indicator == "HTS_TST_POS",
             standardizeddisaggregate == "Modality/Age/Sex/Result",
             fiscal_year <= metadata$curr_fy, 
             ageasentered %in% c("15-19", "20-24")) %>% 
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
      mutate(mod_order = fct_reorder(mod_type, value, .desc = T)) %>% 
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
      )) %>% 
      filter(!is.na(mod_order))    

    
    df_ayp_modality %>% 
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
           title = glue("INDEX TESTING REMAINS THE MOST EFFECTIVE MODALITY TO IDENTIFY POSITIVE AYP"),
           caption = glue("Source: {metadata$caption}")) +
      theme(legend.position = "none") +
      scale_y_continuous(label = label_number_si()) +
      scale_x_discrete(labels = c("FY21Q1", "", "", "",
                                  "FY22Q1", "", "", "FY22Q4")) +
      si_style_ygrid(facet_space =1)     
    
    si_save("Graphics/AYP_modality_trends_.svg", scale = 1.25)
``    