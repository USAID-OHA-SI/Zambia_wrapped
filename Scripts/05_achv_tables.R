# PROJECT: AYP Achievement by IP - table
# PURPOSE: Create FY23 Coverage maps for All of Zambia 
# AUTHOR: Tim Essam | SI
# REF ID:   702b8752
# LICENSE: MIT
# DATE: 2022-10-17
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
  library(gtExtras)
  library(cascade) # Use dev version
  library(ggpattern)
  library(ggtext)
  
  
  # SI specific paths/functions  
  load_secrets()
  merdata <- file.path(glamr::si_path("path_msd"))
  file_path <- return_latest(folderpath = merdata,
                             pattern = "Genie-PSNUByIMs-Zambia")
  
  plhiv_path <- return_latest(folderpath = merdata,
                              pattern = "SUBNAT")

  # REF ID for plots
  ref_id <- "98614ee3"  
  
  # Grab metadata
  get_metadata(file_path)
  
  # Functions  
  source("Scripts/99_custom_functions.r")
  

# MUNGE -------------------------------------------------------------------

  df <- read_msd(file_path) %>% 
    fix_mech_names() %>% 
    clean_agency()
  
  # Looking for a ACHV table by IP across the following
  # indic_list <- c("PrEP_NEW", "HTS_TST_POS", "TX_CURR", "TX_NEW", "VLS", "VLC")
  
  df_achv <- df %>% 
    filter(indicator %in% c("PrEP_NEW", "HTS_TST_POS", "TX_NEW", "TX_CURR"),
           standardizeddisaggregate == "Total Numerator", 
           fiscal_year == metadata$curr_fy, 
           funding_agency != "DEDUP") %>% 
    group_by(fiscal_year, mech_code, mech_name, funding_agency, indicator) %>% 
    summarise(across(matches("cumul|targ"), sum, na.rm = T), .groups = "drop") %>% 
    calc_achievement() %>% 
    adorn_achievement() %>% 
    mutate(tgt_rslt_sum = format_achv(targets, cumulative))
           
  # So, we want to create a similar shaped data frame so we can bind rows. 
  # BUT -- VLS and VLC do not have "targets" so we'll use actual TX_PVLS and TX_CURR #s in place
  
  df_vl <- df %>%
    filter(fiscal_year == metadata$curr_fy, funding_agency != "DEDUP") %>% 
    create_vl_df(mech_name, mech_code, funding_agency) %>% 
    filter(period == metadata$curr_pd) %>% 
    select(-vls_adj) %>% 
    pivot_longer(cols = vlc:vls, 
                 names_to = "indicator",
                 values_to = "achievement") %>% 
      mutate(tgt_rslt_sum = case_when(
      indicator == "vls" ~ format_achv(tx_pvls, tx_pvls_d),
      indicator == "vlc" ~ format_achv(tx_pvls_d, tx_curr_lag2)
    )) %>% 
    adorn_achievement() %>% 
    mutate(indicator = str_to_upper(indicator),
           fiscal_year = metadata$curr_fy, .after = "period") %>% 
    select(-c(period, tx_curr, tx_curr_lag2, tx_pvls, tx_pvls_d))
  
  # Bind'em together so we can plot them in a table
  df_achv_all <- 
    bind_rows(df_achv, df_vl) %>% 
    mutate(indicator = fct_relevel(indicator, c("PrEP_NEW", "HTS_TST_POS", "TX_CURR", "TX_NEW", "VLS", "VLC"))) %>% 
    arrange(mech_name, indicator)  
    
# One Take  
  df_achv_all %>% 
    select(mech_code, mech_name, indicator, achievement, tgt_rslt_sum, achv_color) %>% 
    mutate(achv = percent(achievement, 1), 
           value = format_indicator(achv, tgt_rslt_sum, achv_color)) %>% 
    select(mech_code, mech_name, indicator, value) %>% 
    pivot_wider(names_from = "indicator",
                values_from = "value") %>% 
    filter(mech_code %ni% c("18327", "18304", "85117", 
                            "17400", "17422", "18487",
                            "17410", "160806", "18528")) %>% 
    gt() %>% 
    gt::fmt_markdown(columns = 3:8) %>% 
    sub_missing(missing_text = "-") %>% 
    cols_hide(mech_code)
  
# Standard table w/ colors
  df_ach_all_gt <- df_achv_all %>% 
    mutate(mech_name = str_replace_all(mech_name, "( Follow On| Follow on)", "")) %>% 
    filter(mech_code %ni% c("18327", "18304", "85117", 
                            "17400", "17422", "18487",
                            "17410", "160806", "18528", "82054", "160789")) %>% 
    select(mech_code, mech_name, indicator, achievement, funding_agency) %>% 
    pivot_wider(names_from = "indicator",
                values_from = "achievement")

  


# PEDS MUNGING ------------------------------------------------------------
 
   df_achv_peds <- df %>% 
    filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR"),
           standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Modality/Age/Sex/Result"), 
           fiscal_year == metadata$curr_fy, 
           funding_agency != "DEDUP", 
           trendscoarse == "<15") %>% 
    group_by(fiscal_year, mech_code, mech_name, funding_agency, indicator) %>% 
    summarise(across(matches("cumul|targ"), sum, na.rm = T), .groups = "drop") %>% 
    calc_achievement() %>% 
    adorn_achievement() %>% 
    mutate(tgt_rslt_sum = format_achv(targets, cumulative))


  # BUT -- VLS and VLC do not have "targets" so we'll use actual TX_PVLS and TX_CURR #s in place
  
  df_vl_peds <- df %>%
    filter(fiscal_year == metadata$curr_fy, funding_agency != "DEDUP",
           trendscoarse == "<15") %>% 
    create_vl_df(mech_name, mech_code, funding_agency, trendscoarse) %>% 
    filter(period == metadata$curr_pd) %>% 
    select(-vls_adj) %>% 
    pivot_longer(cols = vlc:vls, 
                 names_to = "indicator",
                 values_to = "achievement") %>% 
    mutate(tgt_rslt_sum = case_when(
      indicator == "vls" ~ format_achv(tx_pvls, tx_pvls_d),
      indicator == "vlc" ~ format_achv(tx_pvls_d, tx_curr_lag2)
    )) %>% 
    adorn_achievement() %>% 
    mutate(indicator = str_to_upper(indicator),
           fiscal_year = metadata$curr_fy, .after = "period") %>% 
    select(-c(period, tx_curr, tx_curr_lag2, tx_pvls, tx_pvls_d))
  
  # Bind'em together so we can plot them in a table
  df_achv_pedsl <- 
    bind_rows(df_achv_peds, df_vl_peds) %>% 
    mutate(indicator = fct_relevel(indicator, c("HTS_TST", "HTS_TST_POS", "TX_CURR", "TX_NEW", "VLS", "VLC"))) %>% 
   filter(mech_code %ni% c("18327", "18304", "85117", 
                                      "17400", "17422", "18487",
                                      "17410", "160806", "18528", "82054", "160789")) %>% 
    arrange(mech_name, indicator)  %>% 
    select(mech_code, mech_name, indicator, achievement, funding_agency) %>% 
    pivot_wider(names_from = "indicator",
                values_from = "achievement")
  
# CREATE GT TABLES --------------------------------------------------------

      
  tmp <- df_achv_pedsl %>% 
    gt(groupname_col = "funding_agency") %>% 
    sub_missing(missing_text = "-") %>% 
    fmt_percent(columns = 3:9, decimals = 0)
    
  
  indic_cols <- names(df_achv_pedsl)[4:9]
  for(i in seq_along(indic_cols)){
   tmp <- tmp %>% 
     tab_style(
      style = cell_fill(color = "#ff939a", alpha = 0.75),
      locations = cells_body(
        columns = indic_cols[i],
        rows = tmp$`_data`[[indic_cols[i]]] < 0.75
      )
     ) %>% 
     tab_style(
       style = cell_fill(color = "#ffcaa2", alpha = 0.75),
       locations = cells_body(
         columns = indic_cols[i],
         rows = tmp$`_data`[[indic_cols[i]]] >= 0.75 &  tmp$`_data`[[indic_cols[i]]] <= 0.89
       )
     ) %>% 
     tab_style(
       style = cell_fill(color = "#5BB5D5", alpha = 0.75),
       locations = cells_body(
         columns = indic_cols[i],
         rows = tmp$`_data`[[indic_cols[i]]] >= 0.9 &  tmp$`_data`[[indic_cols[i]]] <= 1.1
       )
     ) %>% 
     tab_style(
       style = cell_fill(color = "#e6e6e6", alpha = 0.75),
       locations = cells_body(
         columns = indic_cols[i],
         rows = tmp$`_data`[[indic_cols[i]]] > 1.1
       )
     ) %>% 
     tab_style(
       style = cell_borders(color = "#ffffff", weight = px(2)),
       locations = cells_body(
         columns = everything(),
         rows = everything()
       )
     ) %>% 
     cols_width(
       mech_code ~ px(50),
       everything() ~ px(100)
     )
  } 
    
  tmp %>% 
    tab_header(
      title = glue("ZAMBIA MECHANISM PERFORMANCE SUMMARY PEDIATRICS"),
      subtitle = legend_chunk
    ) %>% 
    tab_source_note(
      source_note = gt::md(glue::glue("{metadata$caption}"))
    ) %>% 
    tab_options(
      source_notes.font.size = px(10)
    ) %>% 
    selfdestructin5::bold_rowgroup() %>% 
    cols_label(mech_name = "", 
               mech_code = "") %>% 
    cols_hide(mech_code) %>% 
    gtsave_extra("Images/USAID_summary_table_achv_ped.png")

    
  
  # Partners to show
  # Action HIV, CIRKUITS, EPHO, Jhpiego, LPHO, SAFE, SPHO, UTH, SPHO, Zam-Health
  
  # USAID Only partners -- SAFE, DISCOVER, ACTION HIV,
  
  # Indicator order --> PrEP_NEW, HTS_TST_POS, TX_CURR, TX_NEW, VLS, VLC