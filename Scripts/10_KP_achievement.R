# PROJECT: FY22Q4 Data Review
# PURPOSE: Munge and Analysis of KP data and cascade
# AUTHOR: Tim Essam | SI
# REF ID:   8cdaebb9
# LICENSE: MIT
# DATE: 2022-10-28
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
                           pattern = "Genie-PSNUByIMs-MultipleOUs-Daily-")
#pattern = "Genie-PSNUByIMs-Zambia")

      
  # Grab metadata
   get_metadata(file_path)
  
  # REF ID for plots
    ref_id <- "8cdaebb9"
    
  # Functions  
  

# LOAD DATA ============================================================================  

 df_msd <- read_msd(file_path) %>% 
      filter(funding_agency == "USAID") %>% 
      fix_mech_names() 

# MUNGE ============================================================================
  
  # Pull KP PREV first
  df_kp <- 
      df_msd %>% 
      filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "HTS_TST_NEG",
                              "KP_PREV", "PrEP_CT", "PrEP_CT", "PrEP_NEW"),
             standardizeddisaggregate %in% c("KeyPop", "KeyPop/HIVStatus", 
                                             "KeyPopAbr", "KeyPop/Result"),
             fiscal_year == metadata$curr_fy) %>% 
      clean_indicator() %>% 
      mutate(otherdisaggregate = case_when(
        str_detect(otherdisaggregate, "People in prisons") ~ "People in prisions",
                   TRUE ~ otherdisaggregate
                   )
      )
    
  df_kp_all <- df_kp %>% mutate(otherdisaggregate == "ALL KPs")  
      
  munge_kp <- function(df, ...){   
      df %>% 
      filter(fiscal_year == metadata$curr_fy) %>% 
      group_by(fiscal_year, indicator, otherdisaggregate, standardizeddisaggregate, ...) %>% 
      summarise(across(matches("targ|cumu"), sum, na.rm = T)) %>% 
      ungroup() %>% 
      mutate(achv = cumulative / targets)
  }
    
    df_kp_viz  <- 
      df_kp %>% 
      #bind_rows(., df_kp %>% mutate(otherdisaggregate = "ALL KPs")) %>% 
      filter(indicator != "PrEP_CT", otherdisaggregate != "PWID") %>% 
      munge_kp(mech_code, mech_name) %>% 
      mutate(indicator = fct_relevel(indicator, c("KP_PREV", "HTS_TST", "HTS_TST_POS", "HTS_TST_NEG", "PrEP_NEW")),
             indic_color = case_when(
               indicator == "KP_PREV" ~ "#88CCEE",
               indicator == "HTS_TST" ~ "#DDCC77",
               indicator == "HTS_TST_POS" ~ "#CC6677",
               indicator == "HTS_TST_NEG" ~ "#117733",
               indicator == "PrEP_NEW" ~ "#AA4499"
             ))
  
# VIZ ============================================================================

    df_kp_viz %>% 
      # filter(otherdisaggregate == "ALL KPs") %>%
      filter(str_detect(mech_name, ("ACTION HIV|CHECKUPII"), negate = T)) %>% 
      ggplot(aes(y = indicator)) +
      geom_col(aes(x = targets), fill = grey20k, 
               position = position_nudge(y = -0.15), width = 0.5) +
      geom_col(aes(x = cumulative, fill = indic_color), width = 0.5) +
      geom_errorbar(aes(xmax=targets, xmin=targets), 
                    width=0.75, 
                    size = 0.75, 
                    color= "#ffffff", 
                    linetype = "dotted") +
      geom_text(aes(x = cumulative, label = percent(achv, 1)), 
                size = 10/.pt, 
                family = "Source Sans Pro", 
                hjust = 0) +
      # facet_wrap(mech_name~otherdisaggregate, ncol = 5) +
      facet_grid(mech_name ~ otherdisaggregate, scales = "free_y", space = "free") +
      si_style_xgrid(facet_space = 1.1) +
      scale_fill_identity() +
      scale_y_discrete(limits = rev) +
      scale_x_continuous(labels = label_number_si(), position = "top", expand = c(0.15, 0.1)) +
      labs(x = NULL, y = NULL, title = str_to_upper("Total KP Achievements for FY22"))+
           #caption = metadata$caption) +
      theme(
        strip.placement = "outside",
        strip.text = element_text(face = "bold")
      )
    
    
    df_kp_viz %>% 
      filter(str_detect(mech_name, ("DISCOVER|CHEKUP|Open"))) %>% 
      ggplot(aes(y = indicator)) +
      geom_col(aes(x = targets), fill = grey20k, 
               position = position_nudge(y = -0.15), width = 0.5) +
      geom_col(aes(x = cumulative, fill = indic_color), width = 0.5) +
      geom_errorbar(aes(xmax=targets, xmin=targets), 
                    width=0.75, 
                    size = 0.75, 
                    color= "#ffffff", 
                    linetype = "dotted") +
      geom_text(aes(x = cumulative, label = percent(achv, 1)), 
                size = 10/.pt, 
                family = "Source Sans Pro", 
                hjust = 0) +
      facet_grid(mech_name ~ otherdisaggregate, scales = "free_y", space = "free") +
      si_style_xgrid(facet_space = 1.1) +
      scale_fill_identity() +
      scale_y_discrete(limits = rev) +
      scale_x_continuous(labels = label_number_si(), position = "top", expand = c(0.15, 0.1)) +
      labs(x = NULL, y = NULL, title = str_to_upper("Total KP Achievements for FY22"))+
      #caption = metadata$caption) +
      theme(
        strip.placement = "outside",
        strip.text = element_text(face = "bold")
      )

# SPINDOWN ============================================================================

    top / bottom %>% 
      si_save(glue("Graphics/{metadata$curr_fy}KP_achv.svg"))

              