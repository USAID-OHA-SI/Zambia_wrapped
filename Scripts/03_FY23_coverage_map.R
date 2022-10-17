# PROJECT: Mapa Zambia
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
  library(cascade) # Use dev version
  library(ggpattern)


  # SI specific paths/functions  
  load_secrets()
  merdata <- file.path(glamr::si_path("path_msd"))
  file_path <- return_latest(folderpath = merdata,
                             pattern = "PSNU_IM_FY20-23_20220916_v2_1_Zambia.zip")
  
  plhiv_path <- return_latest(folderpath = merdata,
                              pattern = "SUBNAT")
  
  shpdata <- glamr::si_path("path_vector")
  
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
  
  # Roll it up by mechanism at the PSNU-level
  zmb_targets <- df_msd %>% 
    filter(standardizeddisaggregate == "Total Numerator",
           fiscal_year == 2023, 
           targets > 0, 
           mech_code != "HllvX50cXC0") %>% 
    group_by(mech_code, mech_name, psnu, fiscal_year, snu1, psnuuid, snu1uid,
             funding_agency) %>% 
    summarise(across(targets, sum, na.rm = T), .groups = "drop")

# MUNGE ============================================================================
  
  #  PULL IN PSNU MAPS info
  snu1_geo <- st_read("../Zambezi/GIS/snu1_fy22.shp")
  plot(snu1_geo)

  cntry <- "Zambia"
  spdf_pepfar <- gisr::get_vcpolygons(path = shpdata, name = "VcPepfarPolygons.shp")
  zmb_geo <- purrr::map(3:5, ~spdf_pepfar %>% gisr::extract_boundaries(country = cntry, 
                                                                       level = .x))
  names(zmb_geo) <- list("adm0", "snu1", "psnu")
  
  zmb_targets_geo <- zmb_targets %>% left_join(zmb_geo$psnu, by = c("psnuuid" = "uid")) %>% 
    clean_agency()
    
# VIZ ============================================================================

  zmb_targets_geo %>% 
    filter(funding_agency
           %in% c("USAID", "CDC")) %>% 
    ggplot() +
    geom_sf(data = snu1_geo, fill = grey10k) +
    geom_sf(aes(geometry = geometry, fill = funding_agency), color = "white", size = 0.25) +
    geom_sf(data = snu1_geo, fill = NA) +
    facet_wrap(funding_agency ~mech_code + mech_name, labeller = label_wrap_gen(multi_line=FALSE)) +
    si_style_map(facet_space = 0.25)+
    scale_fill_manual(values = c("USAID" = old_rose, "CDC" = denim)) +
    theme(strip.text.x = element_text(size = 7)) +
    labs(x = NULL, y = NULL, fill = "Funding Agency",
         title = "FY23 Geographic Coverage by Funding Agency",
         caption = metadata$source)
  
  si_save("Images/FY23_map_coverage.svg")

# SPINDOWN ============================================================================

    # Export the data, dropping targets (don't make any sense here)
  write_csv(zmb_targets %>% select(-targets) %>% st_drop_geometry(), "Dataout/FY23_coverage.csv")
