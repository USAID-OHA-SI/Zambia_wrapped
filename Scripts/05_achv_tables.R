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
  library(cascade) # Use dev version
  library(ggpattern)
  library(ggtext)
  library(gt)
  library(selfdestructin5)
  
  # SI specific paths/functions  
  load_secrets()
  merdata <- file.path(glamr::si_path("path_msd"))
  file_path <- return_latest(folderpath = merdata,
                             pattern = "Genie-PSNUByIMs-Zambia")

  # REF ID for plots
  ref_id <- "98614ee3"  
  
  # Grab metadata
  get_metadata(file_path)
  
  # Functions  
  source("Scripts/99_custom_functions.r")
  swap_targets <- function(.data, mech1 = "18304", mech2 = "82075") {
    # Using EQUIP as default as this has to be done each time in FY21
    .data %>%
      mutate(mech_code = ifelse(mech_code == {{mech1}}, {{mech2}}, mech_code),
             mech_name = ifelse(mech_code == {{mech2}}, "Action HIV", mech_name))
  }
  

# MUNGE -------------------------------------------------------------------

  df <- read_msd(file_path) %>% 
    fix_mech_names() %>% 
    clean_agency() %>% 
    swap_targets()
  
  mdb_df   <- make_mdb_df(df)
  mdb_tbl  <- reshape_mdb_df(mdb_df, metadata$curr_pd)  
  
  # Create the treatment data frame needed for derived indicators
  mdb_df_tx    <- make_mdb_tx_df(df)
  mdb_tbl_tx   <- reshape_mdb_tx_df(mdb_df_tx, metadata$curr_pd)
  

# CREATE TABLES -----------------------------------------------------------

  mdb_tbl %>% 
    # filter(indicator != "GEND_GBV") %>%
    create_mdb(ou = "Zambia", type = "main", metadata$curr_pd, metadata$source) %>% 
    gtsave(path = "Images", filename = glue::glue("Zambia_{metadata$curr_pd}_mdb_main.png"))  
  
  
  create_mdb(mdb_tbl_tx, ou = "Zambia", type = "treatment", metadata$curr_pd, metadata$source) %>% 
    bold_column(., Q4) %>% 
    embiggen() %>% 
    gtsave(., path = "Images", filename = glue::glue("{metadata$curr_pd}_Zambia_MMD_VL_MD.png"))    
  

# CREATE PARTNER TABLES ---------------------------------------------------

  mk_ptr_tbl <- function(df, mech_id)  {
    
    ip_mdb <- 
      df %>% 
      filter(mech_code == mech_id) %>% 
      make_mdb_df() %>% 
      reshape_mdb_df(., metadata$curr_pd) 
    
    mech_name <-  
      df %>% 
      filter(mech_code == mech_id) %>%
      distinct(mech_name) %>% 
      pull(mech_name)
    
    
    ip_mdb %>%   
      create_mdb(ou = "Zambia", type = "main", metadata$curr_pd, metadata$source) %>% 
      tab_header(
        title = glue::glue("{mech_name} PERFORMANCE SUMMARY")
      ) %>% 
      gtsave(path = "Images", filename = glue::glue("{mech_name}_mdb_main.png"))
  }
  
  #mk_ptr_tbl(df, 82075) 
  mk_ptr_tbl(df, 17413)
  mk_ptr_tbl(df %>% 
               mutate(mech_name = ifelse(str_detect(mech_name, "DISCOVER-H"), "DISCOVER-H", mech_name)), 17399)
  


# ZAM HEALTH --------------------------------------------------------------

  # ADD CUSTOM TABLE FOR ZAMHEALTH ------------------------------------------
  
  
  mk_ptr_tbl(df, 82086)
  
  
  ip_mdb <- 
    df %>% 
    filter(mech_code == 82086, fiscal_year < 2023) %>% 
    make_mdb_df() %>% View()
    filter(fiscal_year == metadata$curr_fy, operatingunit == "Global") %>% 
    select(agency, indicator, indicator_plain, qtr1, qtr2, qtr3, qtr4, cumulative, targets) %>% 
    mutate(achv = cumulative / targets) %>% 
    mutate_if(is.numeric, list(~na_if(., Inf)))
  
  ip_mdb %>% 
    gt(groupname_col ="agency") %>% 
    gt_merge_stack(col1 = indicator, col2 = indicator_plain,
                   palette = c(grey90k, grey80k),
                   font_size = c("12px", "11px"),
                   font_weight = c("normal", "normal")) %>% 
    cols_align(columns = 2, 
               align = c("left")) %>% 
    cols_label(
      indicator = ""
    ) %>% 
    fmt_number(
      columns = 4:9, 
      decimals = 0
    ) %>% 
    fmt_percent(
      columns = 10,
      decimals = 0
    ) %>% 
    sub_missing(missing_text = "-") %>% 
    tab_header(title = md("Zambia Accessible Markets for Health Performance Summary")) %>% 
    tab_source_note(
      source_note = gt::md(glue::glue("**Source**: {authors_footnote(metadata$source)} | si.coreanalytics@usaid.gov"))
    ) %>% 
    gt::tab_style(
      style = list("font-variant: small-caps;"),
      locations = gt::cells_column_labels(columns = tidyselect::everything())
    ) %>% 
    tab_options(
      source_notes.font.size = 8,
      source_notes.padding = px(1),
      data_row.padding = px(5),
      table.font.size = 13
    ) %>% 
    tab_spanner(
      label = glue::glue("FY22 Summary"),
      columns = 4:9
    ) %>% 
    gtsave(path = "Images", filename = glue::glue("ZamHealth_mdb_main.png"))
  
  
  