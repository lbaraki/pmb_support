# PROJECT: PVT Visuals FY24Q1
# PURPOSE: Munge and Analysis of PMTCT_EID & PMCT_HEI
# AUTHOR:  Lemlem Baraki | SI
# REF ID:  afff9f16
# LICENSE: MIT
# DATE:   2024-02-27
# NOTES:   

# LOCALS & SETUP ============================================================================

  # Libraries
  library(gagglr)
  library(grabr)
  library(tidyverse)
  library(scales)
  library(sf)
  library(systemfonts)
  library(tidytext)
  library(patchwork)
  library(ggtext)
      
    
  # SI specific paths/functions  
    load_secrets()
    merdata <- file.path(glamr::si_path("path_msd"))
    file_path <- return_latest(folderpath = merdata,
                               pattern = "OU_IM_FY22-24.")
                               #pattern = "NAT_SUBNAT_FY21-24")
    hist_path <- return_latest(folderpath = merdata,
                               pattern = "OU_IM_FY15-21.")
                               #pattern = "NAT_SUBNAT_FY15-20")
      
  # Grab metadata
   get_metadata(file_path)
  
  # REF ID for plots
    ref_id <- "afff9f16"
    
  # Functions  
  

# LOAD DATA ============================================================================  

    df_msd <- read_psd(file_path) %>% 
      #filter(stringr::str_detect(indicator, "PMTCT_EID"))
      filter(indicator %in% c("PMTCT_EID", "PMTCT_HEI"))
    names(df_msd)
    
    hist_msd <- read_psd(hist_path) %>% filter(indicator %in% c("PMTCT_EID", "PMTCT_HEI"))
    
    total_msd <- bind_rows(df_msd, hist_msd)
    glimpse(total_msd)
    
    total_msd %>% filter(funding_agency == "USAID",
                         fiscal_year %in% c(2024),
                         standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")
                         ) %>% 
      group_by(indicator, fiscal_year, operatingunit,
             standardizeddisaggregate, #otherdisaggregate, ageasentered,
             cumulative) %>%
      summarise(across(starts_with("qtr"), sum, na.rm = TRUE), #quarterly indicator
                .groups = "drop") %>% View()
    

# MUNGE ============================================================================
  
  #numerator: # infants who had any virologic HIV test (previously "first") by 12 months of age during reporting period
      #standardizeddisaggregate = "Total Numerator" vs "Total Denominator" vs Age/EID" (2024) vs "InfantTest" (2017)
      #otherdisaggregate = when using the 3nd disagg; can select "EID First Test" vs "EID Second Test or more" 
          #ageasentered: "<=02 Months" OR "02 - 12 Months" 
      #otherdisaggregate = when using the 4rd disagg; can select "Infant Test within 2 months of birth" & "Infant Test first  between 2 and 12"
    
  #denominator(calculation): # of HIV-positive pregnant women identified in the reporting period, proxy for # HIV-exposed infants
      #note: sum of PMTCT_STAT_POS (known + newly positive) + HTS_TST_POS (pregnancy/L&D + BF) 
    
  #eid testing coverage  
    eid_cov <- total_msd %>%  
      filter(
        indicator %in% c("PMTCT_EID"),
        funding_agency == "USAID",
        standardizeddisaggregate %in% c("Total Denominator", "Age/EID"),
        #otherdisaggregate %in% c("EID First Test","EID Second Test or more"),
        fiscal_year %in% c(2024)) %>% 
      gophr::clean_indicator() %>% 
      group_by(fiscal_year, indicator, ageasentered, otherdisaggregate) %>%  #include operatingunit and/or standarddisaggregate
      summarise(across(starts_with("qtr"), sum, na.rm = TRUE), #quarterly indicator
                .groups = "drop") %>%
      reshape_msd() %>% #incorporates period and period_type
      unite("indicator", indicator:ageasentered, sep = "_", na.rm =  TRUE) %>% 
      #unite("test_age", otherdisaggregate:ageasentered, sep = "_") %>%  
      pivot_wider(
        names_from = indicator, #names_from = test_age,
        names_glue = "{tolower(indicator)}", #names_glue = "{tolower(test_age)}",
        #values_from = value
        ) %>%
      #mutate_all(~replace_na(., 0)) %>%
      select(-period_type) %>% 
      fill(pmtct_eid_d, .direction = "up") %>% 
      rowwise() %>% 
      mutate(
        age_sum = `pmtct_eid_02 - 12 months` + `pmtct_eid_<=02 months`,
        both_cov = age_sum/pmtct_eid_d, 
       baby_cov = `pmtct_eid_02 - 12 months`/ pmtct_eid_d,
        inf_cov = `pmtct_eid_<=02 months` / pmtct_eid_d) 
      #filter(operatingunit %ni% c("Nigeria", "Tanzania","Ethiopia", "Namibia"))
    
   
    
    vers_2 <- total_msd %>%  
      filter(
        indicator %in% c("PMTCT_EID"),
        funding_agency == "USAID",
        standardizeddisaggregate %in% c("Total Denominator", "Age/EID"),
        #otherdisaggregate %in% c("EID First Test","EID Second Test or more"),
        fiscal_year %in% c(2024)) %>% 
      gophr::clean_indicator() %>% 
      group_by(fiscal_year, indicator, ageasentered, otherdisaggregate) %>%  #include operatingunit and/or standarddisaggregate
      summarise(across(starts_with("qtr"), sum, na.rm = TRUE), #quarterly indicator
                .groups = "drop") %>%
      reshape_msd() %>% #incorporates period and period_type
      unite("indicator", indicator:ageasentered, sep = "_", na.rm =  TRUE) %>% 
      select(-period_type) %>% 
      pivot_wider(
        names_from = otherdisaggregate,
        names_glue = "{tolower(otherdisaggregate)}" ) %>% 
      fill(`NA`, .direction = "up") %>% 
      mutate_all(~replace_na(., 0)) %>% 
      rowwise() %>% 
      mutate(
        #age_sum = `pmtct_eid_02 - 12 months` + `pmtct_eid_<=02 months`,
        second_cov = `eid second test or more`/ `NA`,
        first_cov = `eid first test`/ `NA`) %>% 
      filter(!indicator == "PMTCT_EID_D") %>% 
      rename(pmtct_eid_d = `NA`)
    
    
    #Need PMTCT_EID + PMTCT_HEI
    vers_3 <- total_msd %>% 
      filter(
        indicator %in% c("PMTCT_EID", "PMTCT_HEI"),
        funding_agency == "USAID",
        standardizeddisaggregate %in% c("Age/EID", "Age/Result"),
        #otherdisaggregate %in% c("EID First Test","EID Second Test or more"),
        fiscal_year %in% c(2024)) %>% 
      group_by(fiscal_year, indicator, ageasentered) %>%
      summarise(across(starts_with("qtr"), sum, na.rm = TRUE), #quarterly indicator
                .groups = "drop") %>%
      reshape_msd() %>% 
      select(-period_type) %>% 
      pivot_wider(
        names_from = indicator,
        names_glue = "{tolower(indicator)}" ) 
    
  
# VIZ ============================================================================

  #Image 1: in Tableau 
    
  #Image 2: bar chart of EID Coverage Trends by Testing Frequency
      #x-axis: period/fiscal_year
      #y-axis: EID Coverage (PMTCT_EID)
      #facet_wrap by otherdisaggregate ("first" vs "second+")
    viz0 <- 
      eid_cov %>% 
      filter(!otherdisaggregate == "NA") %>% 
      ggplot(aes(x = period))+
      geom_col(aes(y = pmtct_eid_d), fill = "#287c6f", width = 0.75)+ #Denominator
      geom_col(aes(y = `pmtct_eid_<=02 months`), fill = "#7ecfc0", width = 0.75, position = position_nudge(x = 1))+ #Sum
      geom_col(aes(y = `pmtct_eid_02 - 12 months`), fill = "#7ecfc0", width = 0.75, position = position_nudge(x = 2))+
      geom_col(aes(y = age_sum), fill = "#7ecfc0", width = 0.75, position = position_nudge(x = 3))+
      geom_label(aes(y = `pmtct_eid_<=02 months`,
                    label = percent(inf_cov, 1)),
                size = 11/.pt, 
                family = "Source Sans Pro", 
                color = grey90k,
                position = position_nudge(x = 1),
                vjust = -0.5)+
      geom_label(aes(y = `pmtct_eid_02 - 12 months`,
                     label = percent(baby_cov, 1)),
                 size = 11/.pt, 
                 family = "Source Sans Pro", 
                 color = grey90k,
                 position = position_nudge(x = 2),
                 vjust = -0.5)+
      geom_label(aes(y = age_sum,
                     label = percent(both_cov, 1)),
                 size = 11/.pt, 
                 family = "Source Sans Pro", 
                 color = grey90k,
                 position = position_nudge(x = 3),
                 vjust = -0.25)+
      si_style_ygrid() + 
      facet_wrap(~otherdisaggregate, scales = "free_y")+
      scale_x_discrete(labels = NULL)+
      scale_y_continuous(label = label_number(scale_cut = cut_short_scale())#labels = comma, expand = c(0,0)
                         )+
      coord_cartesian(expand = T) + 
      theme(plot.title = ggtext::element_markdown(),
            plot.caption = ggtext::element_markdown())+
      labs(x = NULL, y = NULL, 
           title = glue::glue("<span style = 'color:#287c6f'>Total Denominator</span> |
                                 <span style = 'color:#7ecfc0'>Testing Age: <=2months, 2-12 months, and 0-12 months</span>"),
           caption = glue::glue("{metadata$caption}"),
           #title = "PMTCT_EID TRENDS FY24"
           )

    si_save("Graphics/PMTCT_EID_testing_cov.png")
    
    #Image 3: bar chart of EID Testing Coverage by Uptake
      #x-axis: period/fiscal_year
      #y-axis: EID Coverage (PMTCT_EID)
      #facet_wrap by testing age group (early = <2 months, late = 2-12 months)
        #info on changing label names: https://ggplot2.tidyverse.org/reference/labellers.html
      
    vers_2$indicator2 <- factor(vers_2$indicator, labels = c("Early Uptake (<=2 months)", "Late Uptake (02-12 months)"))
    
      eid_uptake <- vers_2 %>% 
      #filter(!otherdisaggregate == "NA") %>% 
      rename(pmtct_eid_d = `NA`) %>% 
      ggplot(aes(x = period))+
        geom_col(aes(y = pmtct_eid_d), fill = "#287c6f", width = 0.75) +      
        geom_col(aes(y = `eid first test`), fill = "#7ecfc0", width = 0.75, position = position_nudge(x = 1))+
        geom_col(aes(y = `eid second test or more`), fill = "#7ecfc0", width = 0.75, position = position_nudge(x = 2))+
        geom_label(aes(y = `eid first test`,
                       label = percent(first_cov, 1)),
                   size = 11/.pt, 
                   family = "Source Sans Pro", 
                   color = grey90k,
                   position = position_nudge(x = 1),
                   vjust = -0.5)+
        geom_label(aes(y = `eid second test or more`,
                       label = percent(second_cov, 1)),
                   size = 11/.pt, 
                   family = "Source Sans Pro", 
                   color = grey90k,
                   position = position_nudge(x = 2),
                   vjust = -0.5)+
        si_style_ygrid() + 
        facet_wrap(. ~ indicator2,
                   scales = "free_y"#
                   #, labeller = labeller(.multi_line = F)
                   )+
        scale_x_discrete(labels = NULL)+
        scale_y_continuous(label = label_number(scale_cut = cut_short_scale()))+
        theme(plot.title = ggtext::element_markdown(),
              plot.caption = ggtext::element_markdown())+
        labs(x = NULL, y = NULL, 
             title = glue::glue("<span style = 'color:#287c6f'>Total Denominator</span> |
                                 <span style = 'color:#7ecfc0'>Number of Tests: First test, Second test or more</span>"),
             caption = glue::glue("{metadata$caption}"),
             #title = "PMTCT_EID TRENDS FY24"
             )
      
      si_save("Graphics/PMTCT_EID_testing_frequency.png")
      
      #Image 4: bar chart of Collected vs Returned 
        #facet_wrap by ageasentered 
      
      pmtct_samp <- vers_3 %>% 
        ggplot(aes(x = period))+
        geom_col(aes(y = pmtct_eid), fill = "#287c6f", width = 0.75)+
        geom_col(aes(y = pmtct_hei), fill = "#7ecfc0", width = 0.75, position = position_nudge(x = 0.8))+
        si_style_ygrid()+
        facet_wrap(~ageasentered, scales = "free_y")+
        scale_x_discrete(labels = NULL)+
        scale_y_continuous(label = label_number(scale_cut = cut_short_scale()))+
        theme(plot.title = ggtext::element_markdown(),
              plot.caption = ggtext::element_markdown())+
        labs(x = NULL, y = NULL, 
             title = glue::glue("<span style = 'color:#287c6f'>PMTCT_EID, Collected</span> |
                                <span style = 'color:#7ecfc0'>PMTCT_HEI, Returned</span>"),
             caption = glue::glue("{metadata$caption}"),
             #title = "PMTCT_EID COLLECTED vs PMTCT_HEI RETURNED FY24Q1"
             )
      
      si_save("Graphics/PMTCT_EID_samples.png")
      
# SPINDOWN ============================================================================

