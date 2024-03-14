# PROJECT: Proportion of HEI with an "Uknown" FO
# PURPOSE: Munge and Analysis of PMTCTO_FO
# AUTHOR:  Lemlem Baraki | SI
# REF ID:  bc6e8134
# LICENSE: MIT
# DATE:   2024-03-08
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

    hist_path <- return_latest(folderpath = merdata,
                               pattern = "OU_IM_FY15-21.")
      
  # Grab metadata
   get_metadata(file_path)
  
  # REF ID for plots
    ref_id <- "bc6e8134"
    
  # Functions  
  

# LOAD DATA ============================================================================  
#Q: Trends in various categories of final outcome unknown over time 
      #filter: agency, OU, IM, etc (not site level)
    
    df_msd <- read_psd(file_path) %>% filter(indicator %in% c("PMTCT_FO"))
    names(df_msd)
    
    hist_msd <- read_psd(hist_path) %>% filter(indicator %in% c("PMTCT_FO"))
    
    total_msd <- bind_rows(df_msd, hist_msd)
    glimpse(total_msd)
  
    
    total_msd %>% 
      filter(funding_agency == "USAID",
                         fiscal_year %in% c(2021),
                         standardizeddisaggregate %in% c("Outcome", "Total Numerator")
    ) %>% 
      group_by(fiscal_year, #operatingunit,
               standardizeddisaggregate, otherdisaggregate) %>%
      summarise(across(c("cumulative"), sum, na.rm = TRUE), 
                .groups = "drop") %>% View()
    
    #df_1 <- read_psd(file_path) 
    #hist_1 <- read_psd(hist_path)
    #total_df_2 <- bind_rows(df_1, hist_1)
    
     # total_df_2%>% select(target_age_2024)
      #  filter(funding_agency == "USAID",
       # indicator %in% c("HTS_TST_POS"),#, "HTS_TST_POS"),
        #standardizeddisaggregate %in% c("Modality/Age/Sex/Result"),
        #str_detect(modality, "ANC1"),
         #    fiscal_year %in% c(2017:2023))%>%
      #gophr::clean_indicator() %>% 
      #group_by(fiscal_year,indicator, standardizeddisaggregate, otherdisaggregate, modality) %>%  #include operatingunit
      #summarise(across(starts_with("cumul"), sum, na.rm = TRUE), 
       #         .groups = "drop") %>% 
      #select(indicator, fiscal_year, standardizeddisaggregate, otherdisaggregate, modality, cumulative) %>% View()

# MUNGE ============================================================================
  
  #proportion of unknowns for PMTCT_FO over time (line charts)
      #the "Outcome" disagg has 4 otherdisaggs: "Uknown", "HIV Infected", "HIV-uninfected", "Died" 
      #"Missing" are those who are not accounted for in the Outcome disagg (which should be equal to Numerator)
      #PMTCT_STAT_POS ("Total Numerator" disagg = sum of "Age/Sex/KnownNewResult" for all ages)
      #HTS_TST_POS ("Modality/Age/Sex/Result" disagg + "PMTCT Post ANC1" modality)  
    
    
  #numerator & denominators
    fo_df <- total_msd %>% 
      filter(indicator %in% c("PMTCT_FO"),
        funding_agency == "USAID",
        standardizeddisaggregate %in% c("Outcome", "Total Denominator", "Total Numerator"),
        otherdisaggregate %in% c("HIV-final status unknown", "Other Outcomes: Died",NA),
        fiscal_year %in% c(2017:2023)) %>% 
      gophr::clean_indicator() %>% 
      group_by(fiscal_year,indicator, otherdisaggregate) %>%  #include operatingunit
      summarise(across(starts_with("cumul"), sum, na.rm = TRUE), 
                .groups = "drop") %>%
      unite("indicator", indicator:otherdisaggregate, sep = "_", na.rm =  TRUE) %>% 
      pivot_wider(
        names_from = indicator,
        names_glue = "{tolower(indicator)}",
        values_from = cumulative
      ) %>%
      #mutate_all(~replace_na(., 0)) %>% #replace NA's with zero
      rowwise() %>% 
      mutate(
        num_1 = `pmtct_fo_hiv-final status unknown` + `pmtct_fo_other outcomes: died`,
        num_2 = num_1 + (pmtct_fo_d - pmtct_fo),
        calc_1 = num_1/pmtct_fo_d,
        calc_2 = num_2/pmtct_fo_d) #total HEI w/ missing outcome + uknown and died FO status
    #%>% 
      #filter(operatingunit %ni% c("Nigeria", "Tanzania","Ethiopia", "Namibia")) 
    
  
# VIZ ============================================================================

  #change over time: trends in PMTCT_FO (unknown + died) across years 
      #x-axis: fiscal_year
      #y-axis: % change 
        #values: 1) FO of died + unknown 2) FO died + unknown + missing 3) FO died + unknown + missing + expected to be enrolled 
        #calc 1: % of all HEI with reported FO of Uknown or Died  
        #calc 2: TRUE % of all HEI with Uknown Outcome Status (reported FO of Uknown, Died, or Missing outcome) 
          #numerator: above + Missing (PMTCT_D - PMTCT_N) --> (the unknown and died are a subset of the total numerator)
        #calc 3: #FO Uknown + Died + Missing + expected enrolled 
          #numerator: Above + ((STAT_POS + TST_POS) - PMTCT_D) --> equivalent to PMTCT_EID_D = #HIV-exposed pregnant women 
      
    viz1 <- fo_df %>% 
      ggplot(aes(x = fiscal_year)) +
      geom_line(aes(y = calc_1), color = "#074895", linewidth = 0.5) + 
      geom_point(aes(y = calc_1), color = "#074895")+ 
      geom_line(aes(y = calc_2), color = "#5b82d8", linewidth = 0.5) + 
      geom_point(aes(y = calc_2), color = "#5b82d8")+ 
      #geom_line(color= "#287c6f",linewidth = 0.5) +
      #geom_point(aes(y = percent_fo), color = "#287c6f", size = 2.5) +
      si_style_ygrid()+ #facet_space = 0.5
      geom_text(aes(y = calc_1, 
                label = percent(calc_1, 1)),
                size = 9/.pt,
                vjust = 2.25,
                hjust = .25
      ) + 
      geom_text(aes(y = calc_2, 
                    label = percent(calc_2, 1)),
                size = 9/.pt,
                vjust = -0.25,
                hjust = -.25
      ) +
      #facet_wrap(~operatingunit, scales = "free_y") +
      scale_y_continuous(labels = scales::percent, expand = c (0, 0.15)) + 
      scale_x_continuous(expand = c(0,1), #add buffer room on right 
                         breaks = c(2017, 2019, 2021, 2023),  
                         label = c("FY17", "FY19", "FY21","FY23")) +
      #coord_cartesian(expand = TRUE) +
      theme(plot.subtitle = ggtext::element_markdown())+
      labs(x = NULL, y = NULL, 
           subtitle = glue::glue("<span style = 'color:#074895'>FO: Uknown & Died </span> |
                                 <span style = 'color:#5b82d8'>FO: Uknown, Died & Missing</span>"),
           caption = glue::glue("{metadata$caption}"),
           title = "Percent of HIV-Exposed Infants with Uknown Final Outcome"
           )

# SPINDOWN ============================================================================

