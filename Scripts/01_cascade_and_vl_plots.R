# PROJECT: Create O.W.L.S for OHA SI
# PURPOSE: Munge and Analysis of Uganda Data
# AUTHOR: Tim Essam | SI
# REF ID:   32ff4a25
# LICENSE: MIT
# DATE: 2022-12-13
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
    library(cascade)
    library(selfdestructin5)
    
    
  # SI specific paths/functions  
    load_secrets()
    merdata <- file.path(glamr::si_path("path_msd"))
    file_path <- return_latest(folderpath = merdata,
      pattern = "PSNU_IM_FY20-23_20221114_v1_1_Uganda")
      
  # Grab metadata
   get_metadata(file_path)
  
  # REF ID for plots
    ref_id <- "32ff4a25"
    
  # Functions  
  

# LOAD DATA ============================================================================  

  df_msd <- read_msd(file_path) 
    
# SUMMARY TABLES ============================================================================
  
  # MDBs
    mdb_df   <- make_mdb_df(df_msd)
    mdb_tbl  <- reshape_mdb_df(mdb_df, metadata$curr_pd)
    
    
    mdb_df_tx    <- make_mdb_tx_df(df_msd, resolve_issues = F)
    mdb_tbl_tx   <- reshape_mdb_tx_df(mdb_df_tx, metadata$curr_pd)
    
    create_mdb(mdb_tbl, ou = "Uganda", type = "main", metadata$curr_pd, metadata$source)
    create_mdb(mdb_tbl_tx, ou = "Uganda", type = "treatment", metadata$curr_pd, metadata$source)

# CACADES ============================================================================
    
  # CASCADES
    batch_cascade_plot(df_msd %>% clean_agency() %>% filter(funding_agency == "USAID",),
                       imgpath = "Images/Cascade/USAID", imgtype =".png")

# PEDS INDEX TESTING ----------------------------------------------------------------
    
    munge_modality(df_msd %>% filter(funding_agency == "USAID", trendscoarse == "<15") %>% 
                     mutate(mech_name = "USAID")) %>% 
      plot_modality(.)

# Viral Load Suppression
    df_vl <- df_msd %>% 
      filter(funding_agency == "USAID", trendscoarse == "<15") %>% 
      create_vl_df()

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
      annotate("text", x = 12.5, y = .8, label = "Viral Load\nSuppression",
               color = burnt_sienna, size = 10/.pt,
               hjust = 0.1) +
      annotate("text", x = 12.5, y = 1.09, label = "Viral Load\nCoverage",
               color = denim, size = 10/.pt,
               hjust = 0.1) +
      si_style_nolines() +
      expand_limits(x = c(1, 14), y = c(0.7,1.2)) +
      theme(axis.text.y = element_blank(), 
            axis.text.x = element_blank()) +
      labs(x = NULL, y = NULL)
    
    bottom <- df_vl %>% 
      ggplot(aes(x = period)) +
      geom_col(aes(y = tx_pvls_d), fill = denim) +
      geom_col(aes(y = tx_curr_lag2), fill = grey10k, alpha = 0.5) +
      si_style_ygrid() +
      scale_y_continuous(labels = comma) +
      expand_limits(x = c(1, 14)) +
      labs(x = NULL, y = NULL) +
      # annotate("segment", x = 13, xend = 8.5, y = 3450, yend = 5100, 
      #          color = grey70k) +
      # annotate("text", x = 13, y = 24000, label = "Coverage gap", 
      #          hjust = 0, size = 8/.pt, family = "Source Sans Pro", 
      #          color = grey70k)+
      annotate("text", x = 13, y = 22965, label = "TX_CURR_LAG2", 
               size = 8/.pt, family = "Source Sans Pro", color = grey50k,
               hjust = 0.2) +
      annotate("text", x = 13, y = 25641, label = "TX_PVLS_D", 
               size = 8/.pt, family = "Source Sans Pro", color = denim,
               hjust = 0.2)
    
    top / bottom + plot_layout(heights = c(1, 3)) +
      plot_annotation(title = glue("PEDIATRIC VIRAL LOAD SUMMARY FOR {metadata$curr_fy}"),
                      caption = metadata$caption)
