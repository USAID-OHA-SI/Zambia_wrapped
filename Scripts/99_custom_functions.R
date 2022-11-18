# PROJECT: custom functions for FY22 Q4 review
# PURPOSE: Munge and Analysis of
# AUTHOR: Tim Essam | SI
# REF ID:   707a0b39
# LICENSE: MIT
# DATE: 2022-10-13
# NOTES: Tim Essam | SI

# LOCALS & SETUP ============================================================================
    
# MECH NAMES --------------------------------------------------------------

  # Google drive location of sheet if needed
  gg_id <- "1n62MTmbpLrRCAsc-15o3ODAPlhNhbFsBWzsQ974s2vs"

 mech_names_cw <- 
   tibble::tribble(
     ~mech_code, ~mech_name_short,
        "160806",      "CHECKUPII",
         "17399",       "DISCOVER",
         "17400",   "Eradicate TB",
         "17410",         "Z-CHPP",
         "17413",           "SAFE",
         "17422",     "Open Doors",
         "18159",       "GHSC-PSM",
         "18304",          "EQUIP",
         "18487",       "Stop GBV",
         "82075",     "ACTION HIV",
         "82086",     "Zam Health",
         "85114",         "ECAP I",
         "85117",         "CHEKUP",
         "85120",        "ECAP II",
         "85121",       "ECAP III",
         "86412",           "ZIHA"
     )


# Replaces mechanism names with a shorter version
 fix_mech_names <- function(.data) {
   .data %>%
     dplyr::left_join(mech_names_cw, by = c("mech_code")) %>%
     dplyr::mutate(mech_name = ifelse(
       !is.na(mech_name_short),
       mech_name_short,
       mech_name
     ))
 }  
#
 #df_msd %>% fix_mech_names %>% count(mech_code, mech_name) %>% prinf() 


# NEXT SECTION ------------------------------------------------------------

 create_vl_df <- function(df, ...) {
   df <- df %>%
     filter(
       indicator %in% c("TX_CURR", "TX_CURR_Lag2", "TX_PVLS"),
       standardizeddisaggregate %in% c(
         "Age/Sex/HIVStatus",
         "Age/Sex/Indication/HIVStatus"
       )
     ) %>%
     clean_indicator() %>%
     group_by(indicator, fiscal_year, ...) %>%
     summarise(across(starts_with("qtr"), sum, na.rm = TRUE), 
               .groups = "drop") %>%
     reshape_msd(include_type = FALSE) %>%
     pivot_wider(
       names_from = indicator,
       names_glue = "{tolower(indicator)}"
     ) %>%
     mutate(
       vlc = tx_pvls_d / tx_curr_lag2,
       vls = tx_pvls / tx_pvls_d,
       vls_adj = tx_pvls / tx_curr_lag2
     )
   return(df)
 }
 
 format_achv <- function(x, y){
   str_c(scales::label_number_si(accuracy = 1)(x), 
         scales::label_number_si(accuracy = 1)(y), sep = " / "
         )
 }

 
 format_indicator <- function(x, y, z){
   name <- stringr::word(x, 1)
   name2 <- stringr::word(y, start = 1, end = 3)
   color <- stringr::word(z)

  glue::glue("<div style='line-height:10px'<span style='font-weight:bold;font-size:14px;color:{color}'>{name}</div>
             <div><span style='font-weight:regular;font-size:11px'>{name2}</br></div>")
 }

 
# Create a gt loop to format indicators
 legend_chunk <- gt::md(glue::glue("Achievement legend: <img src= '{selfdestructin5::legend_snapshot}' style='height:15px;'> "))
 

# Extract coordinates -----------------------------------------------------

 pull_coords <- function(cntry, ou_uid, org_lvl, psnu_lvl, baseurl = "https://final.datim.org/"){
   
   print(paste("Running DATIM API for coordinates in", cntry,  Sys.time(),
               sep = " ... "))
   
   paste0(baseurl,
          "api/organisationUnits?filter=path:like:", ou_uid,
          "&filter=level:eq:", org_lvl, "&",
          "&fields=id,path,geometry&paging=false") %>%
     httr::GET(httr::authenticate(glamr::datim_user(),glamr::datim_pwd())) %>%
     httr::content("text") %>%
     jsonlite::fromJSON() %>%
     purrr::pluck("organisationUnits") %>%
     tibble::as_tibble() %>%
     clean_coords(psnu_lvl)
   
 }  
 
 
 # FUNCTION - CLEAN COORDINATES --------------------------------------------
 
 clean_coords <- function(df, psnu_lvl){
   
   #limit only to sites with coordinates
   # df <- dplyr::filter(df, geometry$type == "Point") 
   
   #if no sites, return null
   if(nrow(df) < 1)
     return(NULL)
   
   levels <- df$path %>%
     stringr::str_count("/") %>%
     max()
   
   #identify psnu
   df <- df %>% 
     dplyr::mutate(path = stringr::str_remove(path, "^/")) %>%
     tidyr::separate(path, paste0("orglvl_", seq(1:levels)), sep = "/", fill = "right") %>% 
     dplyr::select(orgunituid = id, geometry,
                   psnuuid = dplyr::ends_with(as.character(psnu_lvl)))
   
   #return uid + lat + long
   df <- df %>% 
     dplyr::mutate(coordinates = geometry$coordinates) %>% 
     dplyr::select(-geometry) %>% 
     tidyr::unnest_wider(coordinates, names_sep = "_") %>% 
     dplyr::rename(longitude = "coordinates_1", latitude = "coordinates_2") %>%
     dplyr::mutate_at(dplyr::vars("longitude", "latitude"), as.double)
   
   return(df)
 } 
 

# Munge Modality -------------------------------------

 munge_modality <- function(df, ...){   
   df_hts_full <- df %>% 
     filter(indicator == "HTS_TST_POS",
            standardizeddisaggregate == "Modality/Age/Sex/Result",
            fiscal_year <= metadata$curr_fy, 
            funding_agency == "USAID", ...) %>% 
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

# PLOT MODALITY -----------------------------------------------------------

 plot_modality <- function(df){
  
  mech_name <- df %>% distinct(mech_name) %>% pull()
  
  df %>% 
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
        title = glue("HTS MODALITY BY {mech_name} "),
        caption = glue("Source: {metadata$caption}")) +
   theme(legend.position = "none") +
   scale_y_continuous(label = comma) +
   scale_x_discrete(labels = c("FY21Q1", "", "", "",
                               "FY22Q1", "", "", "")) +
   si_style_ygrid(facet_space = 0.5)  
 }
 
 # Now can crank out partner plots
 batch_modality_plot <- function(df, ip_code, export = TRUE){
   
   mech_name <- df %>% 
     filter(mech_code == ip_code) %>% 
     distinct(mech_name) %>% 
     pull()
   
   print(mech_name)    
   munge_modality(df, mech_code == ip_code) %>% 
     plot_modality(.)
   
   if(export == TRUE)
     si_save(glue("Graphics/HTS_modality_{mech_name}.svg"))
 }
 

# SWAP EQUIP TARGETS ------------------------------------------------------

 swap_targets <- function(.data, mech1 = "18304", mech2 = "82075") {
   # Using EQUIP as default as this has to be done each time in FY21
   .data %>%
     mutate(mech_code = ifelse(mech_code == {{mech1}}, {{mech2}}, mech_code),
            mech_name = ifelse(mech_code == {{mech2}}, "Action HIV", mech_name))
 } 
 

# RETURN EVERY NTH TICK ---------------------------------------------------
# https://stackoverflow.com/questions/52919899/ggplot2-display-every-nth-value-on-discrete-axis
 every_nth = function(n) {
   return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
 }
 