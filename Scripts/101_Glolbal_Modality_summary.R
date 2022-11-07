# PROJECT: FY22 Q3 Global Review of HTS MODALITIES
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
                           pattern = "MER_Structured_Datasets_OU_IM_FY20-23_20220812")

# Refid
refid <- "0bb2a8eb"

# Grab metadata
get_metadata(file_path)

# Functions  
source("Scripts/99_custom_functions.r")

# Munge Modality -------------------------------------

munge_modality <- function(df, ...){   
  df_hts_full <- df %>% 
    filter(indicator == "HTS_TST_POS",
           standardizeddisaggregate == "Modality/Age/Sex/Result",
           fiscal_year <= metadata$curr_fy,
           trendscoarse == "<15") %>%  
    #funding_agency == "USAID", ...) %>% 
    mutate(mod_type = case_when(
      str_detect(modality, "Index") ~ "Index",
      str_detect(modality, "OtherPITC") ~ "Other PITC",
      str_detect(modality, "PMTCT") ~ "PMTCT",
      modality == "VCT" ~ "VCT",
      str_detect(modality, "SNSMod") ~ "Community SNS",
      TRUE ~ "Other")
    ) %>%
    group_by(fiscal_year, mod_type, mech_name) %>%
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
    complete(mod_type, period, mech_name) %>% 
    group_by(mod_type, mech_name) %>% 
    fill(mod_order, .direction = "up") %>% 
    group_by(period, mech_name) %>% 
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
  return(df_hts_full)
}

# LOAD --------------------------------------------------------------------

 df_msd <- read_msd(file_path)

 df_msd %>% 
   filter(indicator == "HTS_TST_POS",
          standardizeddisaggregate == "Modality/Age/Sex/Result",
          trendscoarse == "<15") %>% 
   group_by(fiscal_year, indicator) %>% 
   summarise(across(starts_with("qtr"), sum, na.rm = TRUE))

 df_modality <- munge_modality(df_msd %>% mutate(mech_code = 123456, mech_name = "ALL"), 
               mech_code == 123456) 

 df_modality %>% count(period)
 df_modality %>% 
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
        title = glue("PEPFAR HTS_TST_POS MODALITY SUMMARY"),
        caption = glue("Source: {metadata$caption}")) +
   theme(legend.position = "none") +
   scale_y_continuous(label = comma) +
   scale_x_discrete(labels = c("FY20Q1", "", "", "",
                               "FY21Q1", "", "", "",
                               "FY22Q1", "", "")) +
   si_style_ygrid(facet_space = 0.5)   
 
 si_save("Images/PEPFAR_HTS_MODALITY_SUMMARY.png", scale =1.25) 
 