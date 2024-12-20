# PROJECT: Peds TX_MMD Data Pull
# PURPOSE: Munge and Analysis of TX_MMD
# AUTHOR:  Lemlem Baraki | SI
# REF ID:  31a80588
# LICENSE: MIT
# DATE:   2024-12-12
# NOTES:   

# LOCALS & SETUP ============================================================================

  # Libraries
    library(gagglr)
    library(tidyverse)
    library(scales)
    library(sf)
    library(systemfonts)
    library(tidytext)
    library(patchwork)
    library(ggtext)
    library(glue)
    library(gt)
    library(gtExtras)
    
    
  # SI specific paths/functions  
    load_secrets()
    merdata <- file.path(glamr::si_path("path_msd"))
    file_path <- return_latest(folderpath = merdata,
      pattern = "OU_IM")
      
  # Grab metadata
   meta <- get_metadata(file_path)
  
  # REF ID for plots
    ref_id <- "31a80588"
    
  # Functions  
    #Create a wide VL data frame 
    create_vl_df <- function(df, ...) {
      df <- df %>%
        filter(
          indicator %in% c("TX_CURR", "TX_PVLS"),
          standardizeddisaggregate %in% c(
            "Age/Sex/HIVStatus",
            "Age/Sex/Indication/HIVStatus"
          )
        ) %>%
        gophr::clean_indicator() %>%
        group_by(indicator, fiscal_year, ...) %>%
        summarise(across(starts_with("qtr"), sum, na.rm = TRUE), 
                  .groups = "drop") %>%
        reshape_msd(include_type = FALSE) %>%
        pivot_wider(
          names_from = indicator,
          names_glue = "{tolower(indicator)}"
        ) %>%
        group_by(...) %>% 
        mutate(
          tx_curr_lag2 = lag(tx_curr, n = 2),
          vlc = tx_pvls_d / tx_curr_lag2,
          vls = tx_pvls / tx_pvls_d,
          #vls_adj = tx_pvls / tx_curr_lag2
        ) %>% 
        ungroup()
      return(df)
    }  

# LOAD DATA ============================================================================  

  #Pediatric TX_MMD for USAID
      df_msd <- read_psd(file_path) %>% 
      clean_agency() 
    
    df_mmd <- df_msd %>% 
      filter(indicator %in% c("TX_CURR"),
             #fiscal_year == meta$curr_fy,
             standardizeddisaggregate %in% c("Age/Sex/ARVDispense/HIVStatus"),
             funding_agency == "USAID", 
             trendscoarse == "<15")
    
    df_vl <- df_msd %>% 
      filter(funding_agency == "USAID",
             trendscoarse == "<15") %>% 
      create_vl_df(operatingunit, trendscoarse)

# MUNGE ============================================================================
  
  #Restructure
   df_reshape <- df_mmd %>% 
      gophr::clean_indicator() %>% 
      group_by(operatingunit, indicator,trendscoarse, otherdisaggregate, fiscal_year) %>% 
      summarise(across(starts_with("qtr"), sum, na.rm = TRUE), 
                .groups = "drop") %>% 
      reshape_msd(include_type = FALSE) %>% 
      select(period, operatingunit, trendscoarse, otherdisaggregate, value) #%>% 
      #mutate(
       # otherdisaggregate = factor(otherdisaggregate, 
        #                           levels = c("ARV Dispensing Quantity - Less than 3 months",
         #                                     "ARV Dispensing Quantity - 3 to 5 months",
          #                                    "ARV Dispensing Quantity - 6 or more months"))
      #)
    
     df_final <- df_reshape %>%
        arrange(operatingunit) %>% 
      mutate(
        otherdisaggregate = factor(otherdisaggregate, 
                                   levels = c("ARV Dispensing Quantity - Less than 3 months",
                                              "ARV Dispensing Quantity - 3 to 5 months",
                                     "ARV Dispensing Quantity - 6 or more months"))
        ) %>%
       rename(indicator = otherdisaggregate) %>% 
        #arrange(otherdisaggregate) %>% 
      #unite("combo", period:otherdisaggregate, sep = "_", na.rm = T) %>% 
      pivot_wider(
        #names_from = otherdisaggregate,
        names_from = indicator, 
        #names_glue = "{tolower(otherdisaggregate)}"
        names_glue = "{tolower(indicator)}"
        #names_from = combo,
        #names_glue = "{tolower(combo)}"
      ) 
    
    #Join dataframes
     combo_df <- full_join(df_final, df_vl, by = c("operatingunit", "period", "trendscoarse")) 
    
    
    View(combo_df)
    
    #Original MMD only for Alex
    df_reshape <- df_mmd %>% 
      gophr::clean_indicator() %>% 
      filter(fiscal_year == meta$curr_fy) %>% 
      group_by(operatingunit, indicator,trendscoarse, otherdisaggregate, fiscal_year) %>% 
      summarise(across(starts_with("qtr"), sum, na.rm = TRUE), 
                .groups = "drop") %>% 
      reshape_msd(include_type = FALSE) %>% 
      select(operatingunit, trendscoarse, period, otherdisaggregate, value) %>% 
    mutate(
     otherdisaggregate = factor(otherdisaggregate, 
                               levels = c("ARV Dispensing Quantity - Less than 3 months",
                                        "ARV Dispensing Quantity - 3 to 5 months",
                                        "ARV Dispensing Quantity - 6 or more months"))
    )
    
    df_final <- df_reshape %>%
      arrange(operatingunit) %>% 
      mutate(
        otherdisaggregate = factor(otherdisaggregate, 
                                   levels = c("ARV Dispensing Quantity - Less than 3 months",
                                              "ARV Dispensing Quantity - 3 to 5 months",
                                              "ARV Dispensing Quantity - 6 or more months"))
      ) %>%
      #rename(indicator = otherdisaggregate) %>% 
      arrange(period) %>% 
      unite("combo", period:otherdisaggregate, sep = "_", na.rm = T) %>% 
      pivot_wider(
        names_from = combo,
        names_glue = "{tolower(combo)}"
      ) 
    
    View(df_final)
    
# VIZ ============================================================================

  #Output - excel 
    #write.csv(df_final, "Dataout/Peds_MMD_FY24_initial.csv", row.names = F)
    #write.csv(df_final, "Dataout/Peds_MMD_FY22-FY24_initial.csv", row.names = F)
    write.csv(df_final, "Dataout/Peds_MMD_FY24_final.csv", row.names = F)
    
    write.csv(df_vl, "Dataout/Peds_VL_FY22-FY24_initial.csv", row.names = F)
    
    #write.csv(combo_df, "Dataout/Peds_MMD_VL_FY22-FY24_initial.csv", row.names = F)
    
    write.csv(combo_df, "Dataout/Peds_MMD_VL_FY22-FY24_clean.csv", row.names = F)
# SPINDOWN ============================================================================

