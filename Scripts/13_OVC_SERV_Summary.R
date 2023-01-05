# PROJECT: POART Dreaming
# PURPOSE: Munge and Analysis of POART Data for AR
# AUTHOR: Tim Essam | SI
# REF ID:   cbf4c1b2
# LICENSE: MIT
# DATE: 2022-11-14
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
                               pattern = "PSNU_IM.+Zambia")
      
  # Grab metadata
   get_metadata(file_path)
   
  # GENIE DETAILS
   # DATIM data as of: 11/13/2022, 23:31:49 UTC
   # Genie report updated: 11/14/2022, 04:36:01 UTC
   # Current period(s): 2021 Target, 2021 Q1, 2021 Q2, 2021 Q3, 2021 Q4, 
   # 2022 Target, 2022 Q1, 2022 Q2, 2022 Q3, 2022 Q4, 2023 Target
  
  # REF ID for plots
    ref_id <- "cbf4c1b2"
    
  # Functions  
  munge_ovc <- function(df, ...){
    df %>% 
      clean_psnu() %>% 
      filter(str_detect(indicator, "OVC_SERV"), standardizeddisaggregate == "Total Numerator") %>% 
      group_by(fiscal_year, indicator, ...) %>% 
      summarize(across(c(targets, cumulative), sum, na.rm = T)) %>% 
      ungroup()
  }
    
    

# LOAD DATA ============================================================================  

  df_msd <- read_msd(file_path) 
  
  df_msd_ecap <- df_msd %>% filter(mech_code %in% c("85114", "85120", "85121"))
  
# OVC_SERVE ============================================================================
  
  df_ovc_ou <- munge_ovc(df_msd) %>% adorn_achievement()
    
  df_ovc_psnu <- munge_ovc(df_msd, psnu) %>% adorn_achievement()
  

  
# VIZ ============================================================================

  # Overall and PSNU VIZ combined show results to targets for FY22
  top <- df_ovc_ou %>% 
    filter(indicator %in% c("OVC_SERV", "OVC_SERV_UNDER_18"), fiscal_year == metadata$curr_fy) %>% 
    ggplot(aes(y = indicator, group = indicator)) +
    #geom_linerange(aes(xmin = 0, xmax = targets), linewidth = 4,) +
    geom_segment(aes(x = 0, xend = targets, yend = indicator), linewidth = 5, lineend = "round", color = grey20k)+
    geom_point(aes(x = cumulative, fill = achv_color), shape = 21, size = 5) +
    geom_text(aes(x = cumulative, label = percent(achievement, 1)),
              vjust = -1,
              size = 10/.pt,
              color = grey90k) +
    facet_wrap(~indicator, scales = "free") +
    scale_fill_identity() +
    scale_x_continuous(label = scales::label_number_si()) +
    si_style_xgrid() +
    labs(x = NULL, y = NULL) +
    theme(axis.text.y = element_blank())
  
bottom <- df_ovc_psnu %>% 
    filter(indicator %in% c("OVC_SERV", "OVC_SERV_UNDER_18"), fiscal_year == metadata$curr_fy,
           psnu != "Katete") %>%
    mutate(high_achv = ifelse(achievement > .90, 0, 1)) %>% 
    group_by(indicator, high_achv) %>% 
    mutate(psnu_order = fct_reorder(psnu, cumulative)) %>% 
    ungroup() %>% 
    ggplot(aes(y = psnu_order, group = indicator)) +
    #geom_linerange(aes(xmin = 0, xmax = targets), linewidth = 4,) +
    geom_segment(aes(x = 0, xend = targets, yend = psnu_order), linewidth = 3, lineend = "round", color = grey20k)+
    geom_vline(xintercept = -10, size = 3, color = "white") +
    geom_vline(xintercept = -0, size = 0.5, color = grey90k) +
    geom_point(aes(x = cumulative, fill = achv_color), shape = 21, size = 3, stroke = 0.25) +
    geom_text(aes(x = cumulative, label = percent(achievement, 1)),
              hjust = -.25,
              size = 8/.pt,
              color = grey90k) +
    facet_grid(high_achv ~ indicator, scales = "free", space = "free") +
    scale_fill_identity() +
    scale_x_continuous(label = scales::label_number_si()) +
    si_style_xgrid() +
    labs(x = NULL, y = NULL, caption = glue::glue("{metadata$source}"))  
 
 top / bottom +
   plot_layout(heights = c(1, 4)) +
   plot_annotation(title = "ZAMBIA REACHED 108% OVERALL OVC_SERV TARGETS IN FY22")
  
 si_save("Images/OVC_summary.svg", scale = 1.25)
  
 
 # TRENDS for OVC U18 + O18
 df_ovc_ou %>% 
   filter(indicator %in% c("OVC_SERV", "OVC_SERV_UNDER_18", "OVC_SERV_OVER_18")) %>% 
   mutate(indic_order = fct_relevel(indicator, c("OVC_SERV", "OVC_SERV_UNDER_18", "OVC_SERV_OVER_18"))) %>% 
   ggplot(aes(x = factor(fiscal_year))) +
    geom_col(aes(y = targets), fill = grey20k, width = 0.5, position = position_nudge(x = -0.2)) +
   geom_col(aes(y = cumulative), fill = scooter_med, width = 0.5, ) +
   geom_text(data = . %>% filter(fiscal_year != 2023), 
              aes(y = cumulative, label = percent(achievement, 1)), size = 10/.pt, 
              family = "Source Sans Pro",
              vjust = -0.15) +
   facet_wrap(~indic_order) +
   scale_y_continuous(labels = label_number_si(), expand = c(0, 0.2), 
                      limits = c(0, 650000)) +
   si_style_ygrid() +
   labs(x = NULL, y = NULL, 
        caption = glue("{metadata$caption}"),
        title = "ZAMBIA ACHIEVED ALL OVC_SERV TARGETS IN FY22") 
 
 si_save("Images/OVC_SERV_trends.png", scale = 1.25)
 

# SPINDOWN ============================================================================

  # NEED DREAMS BREAKDOWN AS WELL
 df_msd_ecap %>% 
   filter(str_detect(indicator, "OVC_SERV")) %>% 
   count(indicator, standardizeddisaggregate, otherdisaggregate)
 
 df_ovc_ecap <- munge_ovc(df_msd_ecap) %>% adorn_achievement()
 
 df_ovc_ecap %>% 
 filter(indicator %in% c("OVC_SERV", "OVC_SERV_UNDER_18", "OVC_SERV_OVER_18")) %>% 
   mutate(indic_order = fct_relevel(indicator, c("OVC_SERV", "OVC_SERV_UNDER_18", "OVC_SERV_OVER_18"))) %>% 
   ggplot(aes(x = factor(fiscal_year))) +
   geom_col(aes(y = targets), fill = grey20k, width = 0.5, position = position_nudge(x = -0.2)) +
   geom_col(aes(y = cumulative), fill = scooter_med, width = 0.5, ) +
   geom_text(data = . %>% filter(fiscal_year != 2023), 
             aes(y = cumulative, label = percent(achievement, 1)), size = 10/.pt, 
             family = "Source Sans Pro",
             vjust = -0.15) +
   facet_wrap(~indic_order) +
   scale_y_continuous(labels = label_number_si(), expand = c(0, 0.2), 
                      limits = c(0, 350000)) +
   si_style_ygrid() +
   labs(x = NULL, y = NULL, 
        caption = glue("{metadata$caption}"),
        title = "THE ECAPS ACHIEVED ALL OVC_SERV TARGETS IN FY22") 
 
 
 si_save("Images/ECAP_OVC_SERV_trends.svg", scale = 1.25)
 
 
 df_msd_ecap %>% 
   filter(str_detect(indicator, "OVC_SERV"), 
          str_detect(otherdisaggregate, c("Active|Graduated|Exited|Transf")),
          standardizeddisaggregate %in% c("Age/Sex/ProgramStatus", "ProgramStatus"),
          fiscal_year == metadata$curr_fy) %>% 
   mutate(otherdisaggregate = case_when(
     str_detect(otherdisaggregate, "Exited|Transf") ~ "Transfer/Exit",
     TRUE ~ otherdisaggregate
   )) %>% 
   group_by(fiscal_year, indicator, otherdisaggregate) %>% 
   summarize(across(c(cumulative), sum, na.rm = T)) %>% 
   ungroup() %>% 
   mutate(disag_fill = case_when(
     str_detect(otherdisaggregate, "Exit") ~ trolley_grey,
     str_detect(otherdisaggregate, "Grad") ~ burnt_sienna,
     str_detect(otherdisaggregate, "Active") ~ denim),
   disag = fct_relevel(otherdisaggregate, c("Transfer/Exit", "Graduated", "Active")),
          ) %>% 
   ggplot(aes(x = factor(fiscal_year), y = cumulative, group = disag, fill = disag_fill)) + 
   geom_col(alpha = 0.85, width = 0.5) +
   scale_fill_identity() +
   geom_text(aes(label = str_c(disag, "\n", comma(cumulative))), 
             position = position_stack(), vjust = 1.5,
             family = "Source Sans Pro", 
             size = 12/.pt) +
   scale_y_continuous(labels = label_number_si()) +
   si_style_ygrid() +
   labs(x = NULL, y = NULL,
        caption = glue("{metadata$caption}"),
        title = "ECAP - OVC_SERV BY PROGRAM STATUS")
 
 si_save("Images/ECAP_OVC_SERV_program_status.svg", scale = 1.25)
 


# POTENTIAL ECAP CASCADE --------------------------------------------------
 #filter(mech_code %in% c("85114", "85120", "85121"))
    
# OVC CASCADE for ECAP ONLy 
 
 # Function to group and sum
 group_sum <- function(.data, ...){
   .data %>% 
     group_by(indicator, fiscal_year, ...) %>% 
     summarise(across(c(targets, starts_with("qtr")), sum, na.rm = TRUE), .groups = "drop")
 }
 
 df  <- df_msd %>% filter(mech_code %in% c("85114", "85120", "85121"))
 
 
 df_msd %>% 
   filter(str_detect(indicator, "OVC_SERV")) %>% 
   count(indicator, standardizeddisaggregate, otherdisaggregate, otherdisaggregate_sub)

 
 df %>% filter(str_detect(indicator, c("OVC_HIVSTAT")), fiscal_year == 2022) %>% 
   # str()
   count(standardizeddisaggregate,categoryoptioncomboname, indicator, fiscal_year) %>% prinf()
 
 df_art <-  df %>% 
   filter(indicator %in% c("OVC_HIVSTAT"), 
          standardizeddisaggregate  %in% c("Age/Sex/ReportedStatus")) %>% 
   separate(categoryoptioncomboname, sep = ", ", into = c("age", "sex", "art_status")) %>% 
   group_sum(type = art_status) %>% 
   filter(type %in% c("Receiving ART Positive"))
 
 # First get OVC_HIVSTAT b/c it's a PITA
 df_hivstat <- 
   df %>% 
   filter(indicator %in% c("OVC_HIVSTAT"), 
          standardizeddisaggregate  %in% c("Age/Sex/ReportedStatus"),
          !is.na(otherdisaggregate)) %>% 
   group_sum(type = otherdisaggregate)
 
 
 df_hivstat2 <- 
   df %>% 
   filter(indicator %in% c("OVC_HIVSTAT_NEG", "OVC_HIVSTAT_POS", "OVC_HIVSTAT"), 
          standardizeddisaggregate  %in% c("Total Numerator", "Total Denominator"),
          fiscal_year == 2022) %>% 
   group_sum(type = standardizeddisaggregate) %>% 
   mutate(type = case_when(
     indicator == "OVC_HIVSTAT_NEG" ~ "OVC_HIVSTAT_NEG", 
     indicator == "OVC_HIVSTAT_POS" ~ "OVC_HIVSTAT_POS",
     TRUE ~ type
   ))
 
 
 # WHAT PSNUS does ECAP operate in?
 ecap_psnus <- df %>% filter(indicator == "OVC_HIVSTAT_POS") %>% 
   distinct(psnu) %>% 
   pull(psnu)
 
 
 # UNDER 15 TX_CURR + OVC ONLY DISTRICTS
 df_tx_psnu <- 
   df_msd %>% 
   filter(operatingunit == "Zambia", 
          indicator %in% c("TX_CURR", "OVC_HIVSTAT_POS"),
          standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Total Numerator"),
          fiscal_year == metadata$curr_fy, 
          psnu %in% ecap_psnus) %>% 
   group_by(indicator, psnu, trendscoarse, snu1, fiscal_year) %>% 
   summarise(across(c(qtr4), sum, na.rm = T)) %>% 
   ungroup() %>% 
   mutate(keep_flag = case_when(
     indicator == "OVC_HIVSTAT_POS" ~ 1,
     indicator == "TX_CURR" & trendscoarse == "<15" ~ 1,
     TRUE ~ 0
   )) %>% 
   filter(keep_flag == 1) %>% 
   select(-trendscoarse) %>% 
   pivot_wider(names_from = indicator, values_from = qtr4) %>% 
   filter(!is.na(OVC_HIVSTAT_POS)) %>% 
   mutate(proxy_coverage = OVC_HIVSTAT_POS / TX_CURR)
 
 df_tx_psnu %>% 
   group_by(fiscal_year) %>% 
   summarise(proxy_cov = sum(OVC_HIVSTAT_POS)/sum(TX_CURR))
 
 
 df_tx <- df_tx_psnu %>% 
   group_by(fiscal_year) %>% 
   summarize(qtr4 = sum(TX_CURR)) %>% 
   mutate(indicator = "TX_CURR",
          type = "TX_CURR <15")
 
 
 
 # In FY22 we no longer can determine how many are on ART for <18s
 df_ovc <- 
   bind_rows(df_hivstat, df_hivstat2, df_tx, df_art) %>% 
   select(-c("qtr1", "qtr3", "qtr2")) %>% 
   mutate(x_group = case_when(
     indicator == "OVC_HIVSTAT" & type == "Total Denominator" ~ "OVC_SERV <18\n Comprehensive Model",
     indicator == "TX_CURR" ~ "TX_CURR <15",
     indicator == "OVC_HIVSTAT" & type == "Undisclosed to IP" ~ "OVC_HIVSTAT\n Undisclosed",
     indicator == "OVC_HIVSTAT" & type == "Receiving ART Positive" ~ "OVC_HIVSTAT\n Receiving ART",
     indicator == "OVC_HIVSTAT" & type == "Total Numerator" ~ "not used",
     TRUE ~ "OVC_HIVSTAT"
   )
   ) %>% 
   # Create a duplicate of 
   mutate(count = c(1, 1, 1, 1, 1, 1, 2, 1, 1)) %>% 
   uncount(count) %>% 
   mutate(row_num = row_number(),
          x_group = ifelse(row_num == 7, "OVC_HIVSTAT_POS", x_group),
          x_order = fct_relevel(x_group, 
                                c("OVC_SERV <18\n Comprehensive Model",
                                  "OVC_HIVSTAT\n Undisclosed",
                                  "OVC_HIVSTAT", 
                                  "TX_CURR <15", 
                                  "OVC_HIVSTAT_POS",
                                  "OVC_HIVSTAT\n Receiving ART")
          ),
          type = fct_relevel(type, 
                             c("Test Not Required", "Undisclosed to IP",
                               "OVC_HIVSTAT_NEG", "OVC_HIVSTAT_POS", 
                               "Total Denominator", "TX_CURR <15", 
                               "Total Numerator")),
          bar_color = case_when(
            x_order == "OVC_SERV <18\n Comprehensive Model" ~ "#30728b",
            type == "Undisclosed to IP" ~ "#e3e2ce",
            type == "OVC_HIVSTAT_POS" ~ "#d77389",
            type == "OVC_HIVSTAT_NEG" ~ "#e6aab9",
            type == "TX_CURR <15" ~ "#5cb6d6",
            type == "Test Not Required" ~ "#ada9cd",
            type == "Receiving ART Positive" ~ "#e9ddff"
          )
   )
 
 
 df_kstatus <- 
   bind_rows(df_hivstat, df_hivstat2, df_tx) %>% 
   select(-c("qtr1", "qtr3", "qtr4", "targets")) %>% 
   filter(indicator %in% c("OVC_HIVSTAT", 
                           "OVC_HIVSTAT_NEG", 
                           "OVC_HIVSTAT_POS")) %>% 
   slice(2, 4, 5, 7) %>% 
   mutate(pivot_flag = ifelse(type == "Total Denominator", "den", "num")) %>% 
   group_by(pivot_flag) %>% 
   summarise(tot = sum(qtr2)) %>% 
   spread(pivot_flag, tot) %>% 
   mutate(proxy = num/den,
          indicator = "OVC_HIVSTAT", 
          row_num = 1)
 
 df_ovc %>% 
   filter(x_group != "not used") %>% 
   ggplot() +
   geom_col(aes(x = x_order, y = qtr4, fill = bar_color)) +
   geom_text(data = . %>% inner_join(df_kstatus),
             aes(x = x_order, y = den,
                 label = paste(percent(proxy, 1), "Known Status Proxy")),
             vjust = -0.1,
             size = 10/.pt,
             family = "Source Sans Pro") +
   geom_text(aes(x = x_order, y = qtr4, label = comma(qtr4)))+
   si_style_xgrid() +
   scale_fill_identity() +
   scale_y_continuous(labels = comma) +
   coord_flip() +
   scale_x_discrete(limits = rev(levels(df_ovc$x_order))) +
   labs(x = NULL, y = NULL,
        caption = glue::glue("{metadata$caption}"))

 
 si_save(glue("Graphics/{metadata$curr_pd}_ZAM_OVC_CASCADE_TX_U15_ECAP.svg"), scale = 1.15) 
 
