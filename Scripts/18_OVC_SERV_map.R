# PROJECT: Zambia Wrapped Peds Analysis
# PURPOSE: Munge and Analysis of OVC_SERV Data
# AUTHOR: Tim Essam | SI
# REF ID:   f8c58060
# LICENSE: MIT
# DATE: 2023-01-06
# NOTES: Tim Essam | SI

# LOCALS & SETUP ============================================================================

# PROJECT: Musonda's request for site level TX_CURR for SAFE
# PURPOSE: Munge and Analysis of SITE LEVEL MSD
# AUTHOR: Tim Essam | SI
# REF ID:   d1a2385e
# LICENSE: MIT
# DATE: 2022-11-28
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
  shpdata <- file.path(glamr::si_path("path_vector"))
  file_path <- return_latest(folderpath = merdata,
                             pattern = "Site_IM_FY20-23.*_Zambia")
  
  # Grab metadata
  get_metadata(file_path)
  
  # REF ID for plots
  ref_id <- "d1a2385e"
  
  # Functions  
  source("Scripts/99_custom_functions.r")
  source("../Zambezi/Scripts/Z02_extract_facilities.R")

# LOAD DATA ============================================================================  

  df_ovc <- read_msd(file_path) %>% 
    filter(indicator == "OVC_SERV", standardizeddisaggregate == "Total Numerator", 
           funding_agency == "USAID")
  
  cntry <- "Zambia"
  spdf_pepfar <- gisr::get_vcpolygons(path = shpdata, name = "VcPepfarPolygons.shp")
  zmb_geo <- purrr::map(3:5, ~spdf_pepfar %>% gisr::extract_boundaries(country = cntry, 
                                                                       level = .x))
  names(zmb_geo) <- list("adm0", "snu1", "psnu")

  zmb_adm1 <- st_read("../Zambezi/GIS/snu1_fy22.shp")
    
# MUNGE ============================================================================
  
  # Match up site lat/long
  ovc_geo <- 
    df_ovc %>% left_join(., gis, by = c("orgunituid" = "id")) %>% 
    filter(fiscal_year == metadata$curr_fy, 
           facility != "Data reported above Facility level")
  
# VIZ ============================================================================

  ovc_geo %>% 
    ggplot() +
    geom_sf(data = zmb_geo$adm0, aes(geometry = geometry), fill = grey10k) +
    geom_sf(data = zmb_adm1, fill = grey10k, alpha = 0.5) +
    geom_point(aes(x = longitude, y = latitude, color = funding_agency), alpha = 0.5) +
    scale_color_manual(values = c("USAID" = old_rose)) +
    si_style_map() +
    labs(x = NULL, y = NULL, 
         caption = glue::glue("Source: {metadata$source} & DATIM API"),
         title = "USAID OVC FACILITIES OF FY22") +
    theme(legend.position = "none")
  
  si_save("Graphics/ZMB_FY22_OVC_SITEs.svg")

# SPINDOWN ============================================================================

