# PROJECT: Create Visuals for FY22 Q4 Data Review
# PURPOSE: Munge and Analysis of Zambia SI Data
# AUTHOR: Tim Essam | SI
# REF ID:   98614ee3
# LICENSE: MIT
# DATE: 2022-10-12
# NOTES: Tim Essam | SI

# LOCALS & SETUP ============================================================================

  # Install dev version of cascade and gophr
  # remotes::install_github("USAID-OHA-SI/gophr", "09ada4c")
  # remotes::install_github("USAID-OHA-SI/cascade", ref = "dev")

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
    #filter(funding_agency == "USAID")
    
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
  return_cascade(df_msd, 1) %>% prinf()
  return_cascade(df_msd %>% filter(funding_agency == "USAID", mech_code == 82075), 1) %>% 
    prinf()
 
  # Generate plots for all agencies
  batch_cascade_plot(df_msd, imgpath = "Images/Cascade/All", imgtype =".svg")
    
  # Generate plots for just USAID
  batch_cascade_plot(df_msd %>% clean_agency() %>% filter(funding_agency == "CDC"),
                     imgpath = "Images/Cascade/cdc", imgtype =".svg")
  
  # Loop over mechs
  batch_cascade_plot(df_msd %>% filter(mech_name == "DISCOVER"),
                     imgpath = "Images/Cascade/DISCOVER", imgtype =".svg")
  
  batch_cascade_plot(df_msd %>% filter(mech_name == "SAFE"),
                     imgpath = "Images/Cascade/SAFE", imgtype =".svg")
  
  batch_cascade_plot(df_msd %>% filter(mech_name == "ACTION HIV"),
                     imgpath = "Images/Cascade/ACTION_HIV", imgtype =".svg")
  
  return_cascade(df_msd %>% filter(mech_name == "DISCOVER"), 13) %>% prinf()
  

# USING CASCADE TRENDS ----------------------------------------------------

  df_spark <- return_cascade(df_msd %>% filter(funding_agency == "USAID", 
                                               mech_name == "SAFE"), 7) %>%
    mutate(results = case_when(
    indicator == "TX_NET_NEW" & period == "FY22Q1" ~ 0,
    TRUE ~ results
    ))
  
  df_spark %>% 
    mutate(start_point = case_when(
      period == min(period) ~ 1,
      TRUE ~ 0
    ),
      end_point = case_when(
        period == max(period) ~ 1,
        TRUE ~ 0
      ),
    ends = ifelse(start_point == 1 | end_point == 1, 1, 0)
    ) %>% group_by(indicator) %>% 
    mutate(min_results = min(results),
           indic_colors = dplyr::case_when(indicator == "HTS_TST" ~ "#877ec9", 
                                           indicator == "HTS_TST_POS" ~ "#b5aaf9", 
                                           indicator == "TX_NEW" ~ glitr::golden_sand_light, 
                                           indicator == "TX_NET_NEW" ~ glitr::golden_sand_light, 
                                           indicator == "TX_CURR" ~ glitr::golden_sand, 
                                           indicator == "TX_PVLS_D" ~ glitr::scooter_med, 
                                           indicator == "TX_PVLS" ~ glitr::scooter),
           indic_group = case_when(
             indicator %in% c("HTS_TST", "HTS_TST_POS") ~ "Testing",
             indicator %in% c("TX_CURR", "TX_NEW", "TX_NET_NEW") ~ "Treatment",
             indicator %in% c("TX_PVLS", "TX_PVLS_D") ~ "Viral Load"
            )
           ) %>% 
    ungroup() %>% 
    filter(indicator != "TX_CURR_Lag2") %>% 
    ggplot(aes(x = period, y = results, group = indicator)) +
    geom_ribbon(aes(ymin = min_results, ymax = results, fill = indic_colors), alpha = 0.25) +
    geom_line(size = 0.75, color = grey70k) +
    geom_point(data = . %>% filter(end_point == 1), shape = 19, color = grey80k, size = 3) +
    geom_point(data = . %>% filter(end_point == 0), shape = 19, color = grey80k, size = 1.5) +
    geom_text(data = . %>% filter(ends == 1), aes(label = label_number_si(accuracy = 1)(results), 
                                                  vjust = -1, size = 12/.pt, 
                                                  family = "Source Sans Pro"))+

    facet_wrap(~indicator, scales = "free_y", ncol = 2) +
    si_style_nolines() +
    scale_y_continuous(labels =  label_number_si(), expand = c(0.5, 0.5)) +
    labs(x = NULL, y = NULL,
         title = "CASCADE TRENDS FOR FY22 FOR ACTION HIV",
         caption = glue("{metadata$caption}")) +
    scale_x_discrete(expand = c(0.05, 0)) +
    theme(axis.text.y = element_blank(), 
          strip.text = element_text(size = 15),
          legend.position  = "none") +
    scale_fill_identity()
  si_save("Images/USAID_cascade_trends_ACTION.png", scale = 1.25)
  
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
  si_save("Graphics/TX_CURR_summary_by_province.svg")
  
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

  si_save("Graphics/TX_CURR_comparison_to_ave.svg") 
  

# TX_CURR PEDS -------------------------------------------------
  
  tx_curr_peds <- df_msd %>% 
    mutate(mech_name = ifelse(mech_name == "EQUIP", "ACTION HIV", mech_name)) %>% 
    filter(funding_agency == "USAID", 
           indicator == "TX_CURR", 
           standardizeddisaggregate == "Age/Sex/HIVStatus",
           trendscoarse == "<15") %>% 
    group_by(mech_name, fiscal_year, indicator) %>% 
    summarise(across(matches("targ|qtr"), sum, na.rm = T)) %>% 
    ungroup() %>% 
    reshape_msd(direction = "quarters") %>% 
    mutate(achv = results_cumulative / targets,
           qtr_flag = ifelse(str_detect(period, "Q4"), 1, 0),
           mech_name = fct_relevel(mech_name, 
                                   c("SAFE", "ACTION HIV", "DISCOVER", "Zam Health")))
  
  
  tx_curr_peds %>% 
    ggplot(aes(x = period)) +
    geom_col(aes(y = targets), fill = grey10k) +
    geom_col(aes(y = results_cumulative), fill = scooter) +
    facet_wrap(~mech_name, nrow = 1,) +
    si_style_ygrid(facet_space = 0.25) +
    scale_x_discrete(labels = c("FY21Q1", "", "", "",
                                "FY22Q1", "", "", "")) +
    scale_y_continuous(labels = comma)+
    geom_text(data = . %>% filter(qtr_flag == 1), 
              aes(y = results_cumulative, label = percent(achv, 1)),
              family = "Source Sans Pro",
              size = 11/.pt, 
              vjust = -.5)+
    labs(x = NULL, y = NULL, title = "TX_CURR PEDIATRIC TRENDS BY PARTNER",
         subtitle = "Gray bars are TX_CURR targets",
         caption = metadata$caption) 
  si_save("Graphics/TX_CURR_pediatric_trends.svg", scale = 1.25)


# TX_CURR PEDS AND PSNU ---------------------------------------------------

  tx_curr_peds_psnu <- df_msd %>%
    swap_targets() %>% 
    mutate(mech_name = str_to_upper(mech_name)) %>% 
    clean_psnu() %>% 
    filter(funding_agency == "USAID",
           indicator == "TX_CURR",
           standardizeddisaggregate == "Age/Sex/HIVStatus",
           trendscoarse == "<15") %>%
    group_by(mech_name, fiscal_year, indicator, psnu, snu1, mech_code) %>%
    summarise(across(matches("targ|qtr"), sum, na.rm = T), .groups = "drop") %>%
    reshape_msd(direction = "quarters") %>% 
    mutate(achv = results_cumulative / targets,
           qtr_flag = ifelse(str_detect(period, "Q4"), 1, 0),
           mech_name = fct_relevel(mech_name,
                                   c("SAFE", "ACTION HIV", "DISCOVER", "ZAM HEALTH")),
           sort_var = case_when(
             period == max(period) ~ results_cumulative,
           )
          ) %>%
    group_by(mech_code, psnu) %>% 
    fill(sort_var, .direction = "up") %>% 
    ungroup() %>%
    group_by(mech_code, snu1) %>%
    mutate(psnu_order = reorder_within(psnu, sort_var, mech_name)) %>%
    ungroup() %>% 
    group_by(mech_code) %>%
    mutate(snu1_order = fct_reorder(snu1, results_cumulative, .fun = sum, .desc = T)) %>%
    ungroup()  
    
    # TRY A SLOPE PLOT, connecting results across years
    # Set a new value that is 1.2 x the value of Q4 results to show achv
    # Calculate 1/2 point between targets and results for placement of achievement
  tx_peds_slope <- function(mech, export = T){
    tx_curr_peds_psnu %>% 
    filter(qtr_flag == 1, mech_name == mech) %>% 
    mutate(tex_pos = case_when(
      period == min(period) ~ 0.8,
      TRUE ~ 2.2
      ),
      achv_pos = case_when(
        period == max(period) ~ results * 1.15,
        TRUE ~ NA_real_
      ),
      psnu = fct_reorder(psnu, results, .desc = T)
    ) %>% 
      adorn_achievement() %>% 
    mutate(achv_lab_pos = abs(targets + results)/2) %>% 
    ggplot(aes(group = psnu, x = period)) +
    geom_ribbon(aes(ymin = results, ymax = targets), fill = grey10k, alpha = 0.25) +
    geom_linerange(aes(ymin = results, ymax = targets,
                       color = ifelse(results > targets, grey10k, grey10k)), 
                   size = 3) +
    #geom_line(aes(y = targets), color = grey80k, linetype = "dotted") +
    #geom_line(aes(y = results), color = grey80k) +
    geom_errorbar(aes(ymin = targets, ymax = targets), size = 0.75, width = 0.1, color = grey80k)+
    #geom_point(aes(y = targets), size = 3, color = grey60k, shape = 15) +
    geom_point(aes(y = results), color = "#6B7B8D", size = 4) +
    geom_text(aes(y = targets, label = label_number_si()(targets), x = tex_pos), 
              size = 8/.pt, family = "Source Sans Pro", 
              color = grey90k) +
    geom_text(aes(y = results, label = label_number_si()(results), x = tex_pos), 
              size = 8/.pt, family = "Source Sans Pro", color = "#6B7B8D") +
    geom_label(data = . %>% filter(period == max(period)), 
                                  aes(y = achv_lab_pos, 
                                      label = percent(achv, 1), 
                                      x = tex_pos,
                                      fill = achv_color,
                                      color = ifelse((results/targets) >= 0.9, grey90k, "white")), 
              size = 7/.pt, family = "Source Sans Pro",
              label.size = NA)+
    # ggrepel:geom_text_repel(aes(y = results, label = percent(achv, 1)), size = 8/.pt, 
                  # family = "Source Sans Pro") +
    # facet_grid(snu1_order~psnu_order, scales = "free", space = "free") +
    facet_wrap(~psnu, scales = "free") +
    scale_color_identity() +
    scale_fill_identity() +
    scale_y_continuous(expand = c(0.1, 0))+
    si_style_nolines(facet_space = 0.5) +
    theme(axis.text.y = element_blank(), 
          axis.text.x = element_text(size = 7, color = grey90k)) +
    labs(x = NULL, y = NULL, 
         title = glue("{mech} PEDIATRIC TX_CURR TARGETS AND RESULTS COMPARISON BY PSNU"),
         caption = glue("{metadata$caption}"))
    
    # if(export == TRUE)
    #     si_save(glue("Graphics/{metadata$curr_pd}_{mech}_TX_CURR_PSNU_PEDS.svg"), scale = 1.33)
  }
  
  
  mech_list <- c("SAFE", "ACTION HIV", "DISCOVER", "ZAM HEALTH")
  map(mech_list, ~tx_peds_slope(.x)) 
    
  tx_peds_slope("DISCOVER", export = F)
  
  
tx_psnu_peds_plot <- function(mech, export = T){
 ip_name <- mech
  tx_curr_peds_psnu %>%
    # filter(period == "FY22Q4", mech_name == "SAFE") %>%
    filter(qtr_flag == 1, mech_name == mech) %>% 
    ggplot(aes(y = psnu_order)) +
    geom_col(aes(x = targets), fill = grey10k) +
    geom_col(aes(x = results_cumulative), fill = scooter) +
    geom_text(aes(x = results_cumulative, label = percent(achv, 1)),
              family = "Source Sans Pro",
              size = 9/.pt,
              hjust = -0.5) +
    facet_grid(~snu1_order ~ period, scales = "free_y", switch = "y", space = "free" )+
    si_style_xgrid(facet_space = 0.5) +
    scale_y_reordered() +
    scale_x_continuous(labels = comma, 
                       expand = expansion(mult = c(0, .05)))+
    theme(strip.text.y.left = element_text(angle = 0),
          strip.placement = "outside", 
          strip.text.x = element_text(face = "bold")) +
   expand_limits(x = c(-0.1, 0)) +
    labs(x = NULL, y = NULL, 
         title = glue("{ip_name} TX_CURR ACHIEVEMENT COMPARISON FY21 TO FY22 FOR PEDIATRICS"),
         caption = glue("{metadata$caption}")
         )
  
    if(export == TRUE)
      si_save(glue("Images/{metadata$curr_pd}_{ip_name}_TX_CURR_PSNU_PEDS.png"))
  }

mech_list <- c("SAFE", "ACTION HIV", "DISCOVER", "ZAM HEALTH")
map(mech_list, ~tx_psnu_peds_plot(.x))
  

# TX_CURR AYP -------------------------------------------------------------

  tx_curr_ayp <- df_msd %>% 
    mutate(mech_name = ifelse(mech_name == "EQUIP", "ACTION HIV", mech_name)) %>% 
    filter(funding_agency == "USAID", 
           indicator == "TX_CURR", 
           standardizeddisaggregate == "Age/Sex/HIVStatus",
           ageasentered %in% c("15-19", "20-24")) %>% 
    group_by(mech_name, fiscal_year, indicator) %>% 
    summarise(across(matches("targ|qtr"), sum, na.rm = T)) %>% 
    ungroup() %>% 
    reshape_msd(direction = "quarters") %>% 
    mutate(achv = results_cumulative / targets,
           qtr_flag = ifelse(str_detect(period, "Q4"), 1, 0),
           mech_name = fct_relevel(mech_name, 
                                   c("SAFE", "ACTION HIV", "DISCOVER", "Zam Health")))
  
  tx_curr_ayp %>% 
    ggplot(aes(x = period)) +
    geom_col(aes(y = targets), fill = grey10k) +
    geom_col(aes(y = results_cumulative), fill = scooter) +
    facet_wrap(~mech_name, nrow = 1,) +
    si_style_ygrid(facet_space = 0.25) +
    scale_x_discrete(labels = c("FY21Q1", "", "", "",
                                "FY22Q1", "", "", "")) +
    scale_y_continuous(labels = comma) +
    geom_text(data = . %>% filter(qtr_flag == 1), 
              aes(y = results_cumulative, label = percent(achv, 1)),
              family = "Source Sans Pro",
              size = 11/.pt, 
              vjust = -.5)+
    labs(x = NULL, y = NULL, title = "TX_CURR AYP (15-24) TRENDS BY PARTNER",
         subtitle = "Gray bars are TX_CURR targets",
         caption = metadata$caption) 
  si_save("Graphics/TX_CURR_ayp_trends.svg", scale = 1.25)
  
# VIRAL LOAD & COVERAGE ---------------------------------------------------

  df_vl <- df_msd %>% 
    filter(funding_agency == "USAID") %>% 
    create_vl_df()
  
  df_vl_peds <- df_msd %>% 
    filter(funding_agency == "USAID", trendscoarse == "<15") %>% 
    create_vl_df()
  
  # Remap mech names so EQUIP becomes ACTION HIV TO SHOW across time
  df_vl_ip <- df_msd %>% 
    filter(funding_agency == "USAID", trendscoarse == "<15") %>%
    mutate(mech_name = ifelse(mech_name == "EQUIP", "ACTION HIV", mech_name)) %>% 
    create_vl_df(mech_name) %>% 
    mutate(mech_name = fct_relevel(mech_name, c("SAFE", "ACTION HIV", "DISCOVER", "Zam Health")))
  
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
    annotate("text", x = 8.5, y = .97, label = "Viral Load\nSuppression",
             color = burnt_sienna, size = 10/.pt,
             hjust = 0.1) +
    annotate("text", x = 8.5, y = .69, label = "Viral Load\nCoverage",
             color = denim, size = 10/.pt,
             hjust = 0.1) +
    si_style_nolines() +
    expand_limits(x = c(1, 10), y = c(0.7,1.05)) +
    theme(axis.text.y = element_blank(), 
          axis.text.x = element_blank()) +
    labs(x = NULL, y = NULL)

  bottom <- df_vl %>% 
    ggplot(aes(x = period)) +
    geom_col(aes(y = tx_curr_lag2), fill = grey10k) +
    geom_col(aes(y = tx_pvls_d), fill = denim) +
    si_style_ygrid() +
    scale_y_continuous(labels = comma) +
    expand_limits(x = c(1, 10)) +
    labs(x = NULL, y = NULL) +
    annotate("segment", x = 8.5, xend = 8.5, y = 345000, yend = 510000, 
             color = grey70k) +
    annotate("text", x = 8.65, y = 450000, label = "Coverage gap", 
             hjust = 0, size = 8/.pt, family = "Source Sans Pro", 
             color = grey70k)+
    annotate("text", x = 9, y = 540000, label = "TX_CURR_LAG2", 
             size = 8/.pt, family = "Source Sans Pro", color = grey50k) +
  annotate("text", x = 9, y = 300000, label = "TX_PVLS_D", 
           size = 8/.pt, family = "Source Sans Pro", color = denim)
  
  top / bottom + plot_layout(heights = c(1, 3)) +
    plot_annotation(title = glue("VIRAL LOAD SUMMARY FOR {metadata$curr_fy}"),
                    caption = metadata$caption)
    
  si_save("Graphics/VL_summary_2022.svg")
  
  # IP VERSION ON 1 GRAPH using small multiples
  top_ip <- 
    df_vl_ip %>% 
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
    si_style_nolines(facet_space = 0.5) +
    facet_wrap(~mech_name, nrow = 1) +
      theme(axis.text.y = element_blank(), 
          axis.text.x = element_blank()) +
    labs(x = NULL, y = NULL) +
    expand_limits(y = c(0.7,1)) 
   
    bottom_ip <- 
      df_vl_ip %>% 
      ggplot(aes(x = period)) +
      geom_col(aes(y = tx_curr_lag2), fill = grey10k) +
      geom_col(aes(y = tx_pvls_d), fill = denim) +
      si_style_ygrid(facet_space = 0.5) +
      scale_y_continuous(labels = comma) +
      labs(x = NULL, y = NULL) +
      facet_wrap(~mech_name, nrow = 1) +
      scale_x_discrete(labels = c("FY21Q1", "", "", "",
                                  "FY22Q1", "", "", "")) +
      coord_cartesian(expand = F) +
      #get rid of facet labels
      theme(strip.text.x = element_blank())+
      labs(caption = metadata$caption)
    
    plot_ip <- top_ip / bottom_ip + plot_layout(heights = c(1, 3)) +
      plot_annotation(title = glue("VIRAL LOAD SUMMARY FOR {metadata$curr_fy} BY PARTNER")) 
    
    si_save("Graphics/VL_summary_partners_peds.svg")
    
  # VLS trends for peds
    top <- df_vl_peds %>% 
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
      annotate("text", x = 8.5, y = .97, label = "Viral Load\nSuppression",
               color = burnt_sienna, size = 10/.pt,
               hjust = 0.1) +
      annotate("text", x = 8.5, y = .69, label = "Viral Load\nCoverage",
               color = denim, size = 10/.pt,
               hjust = 0.1) +
      si_style_nolines() +
      expand_limits(x = c(1, 10), y = c(0.7,1.05)) +
      theme(axis.text.y = element_blank(), 
            axis.text.x = element_blank()) +
      labs(x = NULL, y = NULL)
    
    bottom <- df_vl_peds %>% 
      ggplot(aes(x = period)) +
      geom_col(aes(y = tx_curr_lag2), fill = grey10k) +
      geom_col(aes(y = tx_pvls_d), fill = denim) +
      si_style_ygrid() +
      scale_y_continuous(labels = comma) +
      expand_limits(x = c(1, 10)) +
      labs(x = NULL, y = NULL, caption = metadata$caption) +
      annotate("segment", x = 8.5, xend = 8.5, y = 10200, yend = 17000, 
               color = grey70k) +
      annotate("text", x = 8.65, y = 15000, label = "Coverage gap", 
               hjust = 0, size = 8/.pt, family = "Source Sans Pro", 
               color = grey70k)+
      annotate("text", x = 9, y = 17700, label = "TX_CURR_LAG2", 
               size = 8/.pt, family = "Source Sans Pro", color = grey50k) +
      annotate("text", x = 9, y = 10100, label = "TX_PVLS_D", 
               size = 8/.pt, family = "Source Sans Pro", color = denim) 
    
    top / bottom + plot_layout(heights = c(1, 3)) +
      plot_annotation(title = glue("PEDIATRIC VIRAL LOAD SUMMARY FOR {metadata$curr_fy}"))
    
    si_save("Graphics/VL_summary_peds_2022.svg")
    
    

# AD HOC VL + Proxy Retention ============================================================================
    
    # Used clipr to save to clipboard and deliver to google sheet
    
    df_vl <- df_msd %>% 
      filter(ageasentered %in% c("20-24", "25-29", "30-34"),
             sex == "Male") %>% 
      create_vl_df(ageasentered, sex) %>% 
      arrange(sex, ageasentered, period)
    
    
    df_cont_tx <- 
      df_msd %>% 
      filter(indicator %in% c("TX_NEW", "TX_CURR", "TX_ML"),
             standardizeddisaggregate %in% c("Age Aggregated/Sex/HIVStatus", 
                                             "Age/Sex/HIVStatus", 
                                             "Age/Sex/ARTNoContactReason/HIVStatus"),
             ageasentered %in% c("20-24", "25-29", "30-34"),
             sex == "Male") %>% 
      group_by(indicator, sex, ageasentered, otherdisaggregate, fiscal_year) %>% 
      summarise(across(starts_with("qtr"), sum, na.rm = TRUE), 
                .groups = "drop") %>%
      reshape_msd(include_type = FALSE) %>% 
      mutate(keep_flag = case_when(
        indicator %in% c("TX_CURR", "TX_NEW") ~ 1, 
        indicator == "TX_ML" & otherdisaggregate == "No Contact Outcome - Transferred Out" ~ 1, 
        TRUE ~ 0
      ),
      indicator = ifelse(indicator == "TX_ML", "TX_ML_TO", indicator)) %>% 
      filter(keep_flag == 1) %>% 
      select(-otherdisaggregate) %>% 
      spread(indicator, value) %>% 
      group_by(sex, ageasentered) %>% 
      mutate(TX_CURR_LAG1 = lag(TX_CURR, n = 1), .after = "TX_CURR") %>% 
      mutate(PROXY_RETENTION = TX_CURR / (TX_CURR_LAG1 + TX_NEW))



# AD HOC TX_NEW by fine age bands / partner -------------------------------

    tx_curr_fineage <- df_msd %>% 
      mutate(mech_name = ifelse(mech_name == "EQUIP", "ACTION HIV", mech_name)) %>% 
      filter(funding_agency == "USAID", 
             indicator == "TX_NEW", 
             standardizeddisaggregate == "Age/Sex/HIVStatus",
             trendscoarse == "<15") %>% 
      group_by(mech_name, fiscal_year, indicator, age_2019) %>% 
      summarise(across(matches("targ|qtr"), sum, na.rm = T)) %>% 
      ungroup() %>% 
      reshape_msd(direction = "quarters") %>% 
      mutate(achv = results_cumulative / targets,
             qtr_flag = ifelse(str_detect(period, "Q4"), 1, 0),
             mech_name = fct_relevel(mech_name, 
                                     c("SAFE", "ACTION HIV", "DISCOVER", "Zam Health")))
    
    
    tx_curr_fineage %>% 
      ggplot(aes(x = period, group = mech_name)) +
      geom_line(aes(y = results), color = scooter_med) +
      geom_point(aes(y = results), color = scooter_med) +
      facet_grid(mech_name ~ age_2019, scales = "fixed", space = "free", switch = "y") +
      geom_text(aes(y = results, label = comma(results)), size = 8/.pt, 
                    family = "Source Sans Pro", 
                    vjust = -0.5) +
      si_style_ygrid() +      
      scale_x_discrete(labels = c("FY21Q1", "", "", "Q4", "FY22Q1", "", "", "Q4")) +
      labs(x = NULL, y = NULL, title = str_to_upper("Pediatric TX_NEW quaterly results by partner"),
           caption = metadata$caption) +
      theme(
        strip.placement = "outside",
        strip.text = element_text(face = "bold")
      ) +
      scale_y_continuous(limits = c(0, 150))
    
    si_save("Images/TX_NEW_fineage_partner.png", scale = 1.25)
      
