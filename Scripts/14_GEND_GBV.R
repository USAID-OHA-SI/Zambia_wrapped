# PROJECT: POART DREAMING 
# PURPOSE: Munge and Analysis of GEND GBV Data
# AUTHOR: Tim Essam | SI
# REF ID:   83bc6666
# LICENSE: MIT
# DATE: 2022-11-15
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
      pattern = "Genie-PSNUByIMs-Zambia-Daily")
    
    shpdata <- glamr::si_path("path_vector")
      
  # Grab metadata
   get_metadata(file_path)
  
  # REF ID for plots
    ref_id <- "83bc6666"
    
  # Functions  
  munge_gbv <- function(...){
    df_msd %>% 
      filter(indicator == "GEND_GBV", 
             standardizeddisaggregate == "Total Numerator") %>% 
      group_by(fiscal_year, indicator, ...) %>% 
      summarise(across(matches("targ|cumul"), sum, na.rm = T)) %>% 
      ungroup() %>% 
      adorn_achievement()
  }

# LOAD DATA ============================================================================  

  df_msd <- read_msd(file_path) %>% 
      filter(str_detect(indicator, "GEND"))
    
  
# MUNGE ============================================================================
  
  # Trends
   gbv_ou <- munge_gbv()  
    
   gbv_agency <- munge_gbv(funding_agency) %>% clean_agency()    
   
   gbv_dreams <- munge_gbv(dreams, psnu, psnuuid) %>% 
     mutate(dreams = ifelse(dreams == "Y", "DREAMS DISTRICTS", "NON-DREAMS DISTRICTS"))
     
   gbv_dreams_ou <- munge_gbv(dreams) %>% 
     mutate(dreams = ifelse(dreams == "Y", "DREAMS DISTRICTS", "NON-DREAMS DISTRICTS"),
            dreams_color = ifelse(dreams != "DREAMS DISTRICTS", golden_sand, genoa))
   
  # Pull in map data
   
   spdf_pepfar <- gisr::get_vcpolygons(path = shpdata, name = "VcPepfarPolygons.shp")
   zmb_geo <- purrr::map(3:5, ~spdf_pepfar %>% 
                           gisr::extract_boundaries(country = "Zambia", level = .x))
   names(zmb_geo) <- list("adm0", "snu1", "psnu")
  
   gbv_geo <- 
     gbv_dreams %>% left_join(zmb_geo$psnu, by = c("psnuuid" = "uid"))
   
  # PEP RESULTS
   gbv_viol <- df_msd %>% 
     filter(standardizeddisaggregate == "Age/Sex/ViolenceType") %>% 
     group_by(fiscal_year, indicator, sex) %>% 
     summarise(across(matches("targ|cumul"), sum, na.rm = T)) %>% 
     ungroup()
   
  
# VIZ ============================================================================

  # TRENDS GEND_GBV
   
  top <-  gbv_ou %>% 
     mutate(facet = "Zambia") %>% 
     ggplot(aes(x = factor(fiscal_year))) +
     geom_col(aes(y = targets), fill = grey20k, width = 0.5, position = position_nudge(x = -0.15)) +
     geom_col(aes(y = cumulative), fill = golden_sand, width = 0.5) +
     geom_text(data = . %>% filter(fiscal_year != 2023),
               aes(y = cumulative, label = percent(achievement, 1)), 
                   size = 10/.pt, 
                   family = "Source Sans Pro", 
                   vjust = -0.2) +
     scale_y_continuous(labels = label_number_si(), limits = c(0, 55000)) +
     facet_wrap(~facet)+
     si_style_ygrid() +
     labs(x = NULL, y = NULL)
   
   bottom <- gbv_agency %>% 
     filter(funding_agency != "DEDUP") %>% 
     mutate(funding_agency = fct_relevel(funding_agency, c("USAID", "CDC", "DOD"))) %>% 
     ggplot(aes(x = factor(fiscal_year))) +
     geom_col(aes(y = targets), fill = grey20k, width = 0.5, position = position_nudge(x = -0.15)) +
     geom_col(aes(y = cumulative), fill = golden_sand, width = 0.5) +
     geom_text(data = . %>% filter(fiscal_year != 2023), 
               aes(y = cumulative, label = percent(achievement, 1)), 
               size = 10/.pt, 
               family = "Source Sans Pro", 
               vjust = -0.2) +
     scale_y_continuous(labels = label_number_si()) +
     facet_wrap(~funding_agency) +
     si_style_ygrid() +
     labs(x = NULL, y = NULL, subtitle = "GEND_GBV achievement by agency",
          caption = glue("{metadata$caption}"))
   
   top / bottom +
     plot_layout(heights = c(2, 3)) + 
     plot_annotation(title = "ZAMBIA GEND_GBV ACHIEVEMENT TRENDS", subtitle = "Targets in gray, results in yellow")
   si_save(glue("Images/{metadata$curr_pd}_GEND_GBV_trends.png"), scale = 1.25)


# DREAMS MAP --------------------------------------------------------------

  # ggplot() +
  #    geom_sf(data = zmb_geo$adm0, aes(geometry = geometry), fill = "white", alpha = 0.5) +
  #    geom_sf(data = zmb_geo$psnu, aes(geometry = geometry), fill = "white", alpha = 0.5) +
  #    geom_sf(data = gbv_geo %>% filter(fiscal_year == metadata$curr_fy),
  #            aes(fill = achv_color, geometry = geometry)) +
  #    # geom_sf_text(data = gbv_geo %>% filter(fiscal_year == metadata$curr_fy),
  #    #         aes(label = percent(achievement, 1), geometry = geometry), 
  #    #         size = 8/.pt,
  #    #         family = "Source Sans Pro") +
  #    scale_fill_identity()
   
# BY DREAMS PSNUS
  gbv_dreams_ou %>% 
    ggplot(aes(x = factor(fiscal_year))) +
    geom_col(aes(y = targets), fill = grey20k, width = 0.5, position = position_nudge(x = -0.15)) +
    geom_col(aes(y = cumulative, fill = dreams_color), width = 0.5) +
    geom_text(data = . %>% filter(fiscal_year != 2023), 
              aes(y = cumulative, label = percent(achievement, 1)), 
              size = 10/.pt, 
              family = "Source Sans Pro", 
              vjust = -0.2) +
    scale_y_continuous(labels = label_number_si()) +
    facet_wrap(~dreams) +
    scale_fill_identity() +
    si_style_ygrid() +
    labs(x = NULL, y = NULL, subtitle = "GEND_GBV ACHIEVEMENT BY DREAMS AND NON-DREAMS DISTRICTS",
         caption = glue("{metadata$caption}"))

  si_save(glue("Images/{metadata$curr_pd}_GEND_GBV_dreams.png"), scale = 1.25)
  

# VIOLENCE TYPE -----------------------------------------------------------

 df_msd %>% 
    filter(standardizeddisaggregate %in% c("Age/Sex/ViolenceType", "Age/Sex/PEP")) %>% 
    group_by(fiscal_year, indicator, sex, standardizeddisaggregate) %>% 
    summarise(across(matches("targ|cumul"), sum, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(type = ifelse(standardizeddisaggregate == "Age/Sex/PEP", "PEP", "Violence Type")) %>% 
    ggplot(aes(x = factor(fiscal_year), y = cumulative)) +
    geom_col(aes(fill = type), alpha = 0.85, width = 0.5) +
    facet_grid(type ~ sex, scales = "free_y", switch = "y") +
    scale_fill_manual(values = c("PEP" = golden_sand, "Violence Type" = denim )) +
    si_style_ygrid() +
    scale_y_continuous(labels = label_number_si()) +
    labs(x = NULL, y = NULL, title = "GEND_GBV RESULTS FOR PEP & VIOLENCE TYPE BY SEX",
         caption = glue("{metadata$caption}"), 
         fill = "")  +
    theme(
      strip.placement = "outside",
      strip.text = element_text(face = "bold"), 
      legend.position = "none"
    ) 
  
  
  si_save(glue("Images/{metadata$curr_pd}_GEND_GBV_violence_type.png"), scale = 1.25)
  
df_msd %>% 
    filter(standardizeddisaggregate == "Total Numerator", indicator != "GEND_GBV") %>% 
    group_by(fiscal_year, indicator) %>% 
    summarise(across(matches("targ|cumul"), sum, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(indicator = ifelse(str_detect(indicator, "Sex"), "Sexual Violence", "Physical Emotional Violence")) %>% 
    ggplot(aes(x = factor(fiscal_year), y = cumulative), ) +
    geom_col(fill = burnt_sienna,  width = 0.5) +
    facet_grid(indicator~., switch = "y") +
    si_style_ygrid() +
    scale_y_continuous(labels = label_number_si()) +
    labs(x = NULL, y = NULL, title = "GEND_GBV RESULTS BY VIOLENCE TYPE",
         caption = glue("{metadata$caption}"))  +
    theme(
      strip.placement = "outside",
      strip.text = element_text(face = "bold")
    ) 
 si_save(glue("Images/{metadata$curr_pd}_GEND_GBV_violence_types.png"), scale = 1.25)

  left / right 
