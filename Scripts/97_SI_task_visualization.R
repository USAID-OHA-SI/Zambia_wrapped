# PROJECT: Visualize SI & M&E Responsibilities
# PURPOSE: Munge and Analysis of Tasks
# AUTHOR: Tim Essam | SI
# REF ID:   fa088bb5
# LICENSE: MIT
# DATE: 2022-11-06
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
    library(googlesheets4)
    
    
  # SI specific paths/functions  
    load_secrets()
    gd_id <- "1uPFL57VxyaZaAnsLrVPQmtkc2jAW2Stn3A3ftovj52k"  
    
  # Grab metadata
   #get_metadata(file_path)
  
  # REF ID for plots
    ref_id <- "fa088bb5"
    
  # Functions  
  

# LOAD DATA ============================================================================  

  df_tasks <- read_sheet(ss = gd_id, sheet = "SI Task Matrix") %>% 
      mutate(across(.cols = 3:13, ~str_replace_all(.x, "x|X", "1"))) %>% 
      pivot_longer(cols = 3:13, 
                   names_to = "person", 
                   values_to = "coverage") %>% 
      mutate(fill_color = case_when(
        coverage == "1" ~ scooter_med,
        TRUE ~ grey20k
      ))  %>% 
      mutate(person_order = fct_relevel(person, c("Amanzi", "Amara", "Arthur", "Mwila K", "Yvonne",
                                                         "Henry", "Maurice P", "Mwila C", "Noah",
                                                         "Vincent", "Isaac"))
                    )
    

# MUNGE ============================================================================
  
  df_tasks %>% 
      ggplot(aes(x = person_order, y = task, fill = fill_color)) +
      geom_tile(color = "white") +
      facet_grid(focus~., scales = "free_y", space = "free", switch = "y") +
      scale_fill_identity() +
      scale_x_discrete(position = "top") +
      scale_y_discrete(limits = rev) +
      si_style_nolines(facet_space = 0.25) +
      theme(
        strip.placement = "outside",
        strip.text = element_text(face = "bold")
      ) +
      labs(x = NULL, y = NULL)
    
    si_save("Graphics/SI_task_matrix.svg", scale = 1.25)
     
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================

