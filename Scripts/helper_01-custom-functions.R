# PROJECT: 
# PURPOSE: Helper Functions
# AUTHOR: Tim Essam | SI
# REF ID:   10f2ddaa
# LICENSE: MIT
# DATE: 2022-12-13
# NOTES: Tim Essam | SI

# FUNCTIONS ============================================================================

# For munging index testing modalities
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
    scale_x_discrete(labels = c("FY20Q1", "", "", "",
                                "FY21Q1", "", "", "",
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


# VLS/VLC ============================================================================  


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


