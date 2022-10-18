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
        "160806",           "ZIHA",
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
 legend_chunk <- gt::md(glue::glue("Achievement legend: <img src= '{legend_snapshot}' style='height:15px;'> "))
 