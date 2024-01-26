# PROJECT: PMB + Zambia
# PURPOSE: Munge and Analysis of PMTCT_FO indicator
# AUTHOR: Lemlem Baraki | SI
# REF ID:   df195e18
# LICENSE: MIT
# DATE: 2024-01-26
# NOTES: Lemlem Baraki | SI

# LOCALS & SETUP ============================================================================

  # Libraries
  library(gagglr)
  library(tidyverse)
  library(scales)
  library(systemfonts)
  library(ggtext)
  
    
    
  # SI specific paths/functions  
load_secrets()
merdata <- file.path(glamr::si_path("path_msd"))
file_path <- return_latest(folderpath = merdata,
                           pattern = "PSNU_IM_.*Zambia")

hist_path<- return_latest(folderpath = merdata,
                               pattern = "PSNU_IM_FY15-20.*")
      
  # Grab metadata
get_metadata(file_path)  
  
  # REF ID for plots
    ref_id <- "df195e18"
    
  # Functions  
  

# LOAD DATA ============================================================================  

    df_pmtct <- read_psd(file_path) %>% filter(indicator %in% c("PMTCT_FO")) %>% 
      mutate(snu1 = str_remove_all(snu1, "Province")) %>%
      clean_agency()
    
    #hist_pmtct <- read_psd(file_path_old) %>% filter(operatingunit == "Zambia",
     #                                                  indicator %in% c("PMTCT_FO"),
      #                                                 fiscal_year %in% c(2017:2020)
    #)

# MUNGE ============================================================================
  
    #numerator
    #infected + uninfected from otherdisaggregate 
    num_pmtct <- df_pmtct %>%  
      filter(
        #indicator %in% c("PMTCT_FO"),
        funding_agency %in% c("USAID", 
                              "CDC"),
        standardizeddisaggregate %in% c(
          "Outcome"),
        otherdisaggregate %in% c(
          #"HIV-final status unknown",
          #"Other Outcomes: Died",
          "HIV-uninfected",
          "HIV-infected"),
        fiscal_year %in% c(2017:2023)) %>% 
      group_by(fiscal_year, standardizeddisaggregate, otherdisaggregate,
               funding_agency, snu1) %>%  #include operatingunit
      summarise(across(c("cumulative"), sum, na.rm = TRUE), 
                .groups = "drop") %>%
      pivot_wider(
        names_from = otherdisaggregate,
        names_glue = "{tolower(otherdisaggregate)}",
        values_from = cumulative
      ) %>%
      mutate_all(~replace_na(., 0)) %>% #replace NA's with zero
      rowwise() %>% 
      mutate(
        num_sum = `hiv-infected` + `hiv-uninfected`) #%>% prinf() #%>% arrange(desc(snu1))
    
    #denominator
    denom_pmtct <- df_pmtct %>%  
      filter(
        #indicator %in% c("PMTCT_FO"),
        funding_agency %in% c("USAID","CDC"),
        standardizeddisaggregate %in% c(
          "Total Numerator"),
        fiscal_year %in% c(2017:2023)) %>% 
      group_by(fiscal_year, standardizeddisaggregate, funding_agency, snu1) %>% #include operating unit
      summarise(across(c("cumulative"), sum, na.rm = TRUE), 
                .groups = "drop") #%>% arrange(desc(snu1))
    
    
    #join (Infected + Uninfected/Total Numerator)
    merged_df <- left_join(num_pmtct, denom_pmtct %>% select(fiscal_year, funding_agency, snu1, cumulative),
                           by = c("fiscal_year", "funding_agency", "snu1")) %>% #select(-standardizeddisaggregate) %>% 
      mutate(percent_fo = num_sum/cumulative) 
    
    
    
    # VIZ ============================================================================
    
    merged_df <- merged_df %>% 
      mutate(agency_color = ifelse(funding_agency == "USAID", "#1e87a5","#e07653")) 
    
    
    #Viz 1 - PMTCT_FO trends across FY (infected + uninfected/total numerator)
    #include SNU for small multiples 
    
    #viz1 <-
    merged_df %>% 
      mutate(snu1_agency = fct_reorder(snu1, funding_agency, .desc = TRUE)) %>% #sort the SNU1s by agency
      ggplot(aes(x = fiscal_year, 
                 y = percent_fo,
                 color = agency_color #assign color based on new column
      )) +
      geom_line(data = . %>% group_by(snu1_agency) %>%filter(funding_agency == "USAID"), linewidth = .5) + #USAID vs CDC colors
      geom_line(data = . %>% group_by(snu1_agency) %>% filter(funding_agency == "CDC"), linewidth = .5)+
      geom_point(size = 2.5) +
      scale_color_identity()+
      si_style_ygrid(facet_space = 0.5)+
      geom_text(data = . %>% group_by(snu1_agency) %>%
                  filter(percent_fo %in% c(min(percent_fo),max(percent_fo))),
                aes(label = percent(percent_fo, 1)),
                size = 8/.pt,
                vjust = -0.25,
                hjust = -.25
      ) + 
      facet_wrap(~snu1_agency, #snu1,
                 nrow = 3,
                 scales = "free_y") +
      scale_y_continuous(labels = scales::percent, expand = c (0, 0.15)) +
      scale_x_continuous(expand = c(0,.75),
                         breaks = c(2021,2022,2023),  
                         label = c("FY21", "FY22", "FY23")
      ) +
      #coord_cartesian(expand = TRUE) +
      theme(plot.subtitle = ggtext::element_markdown(),
            plot.margin = )+
      labs(x = NULL, y = NULL, 
           subtitle = glue::glue("<span style = 'color:#1e87a5'>USAID</span>|<span style = 'color:#e07653'>CDC</span>"),
           caption = glue::glue("{metadata$caption} |
                                Known Status = HIV-infected + HIV-uninfected"),
           title = "Proportion of Infants with Known Status")
    
    si_save("Images/ZMB_PMTCT_FO_FY23.png")
    
    #Viz 2 - PMTCT_FO Volume for USAID across FY 
    
    merged_df %>% 
      filter(funding_agency == "USAID") %>% 
    ggplot(aes(x = fiscal_year))+
      geom_col(aes(y = cumulative), fill = "#287c6f", width = 0.75, alpha = 0.75)+ #Numerator
      geom_col(aes(y = num_sum), fill = "#7ecfc0", width = 0.75, position = position_nudge(x = 0.1))+ #Sum
      geom_text(aes(y = num_sum,
                    label = percent(percent_fo, 1)),
                size = 9/.pt, 
                family = "Source Sans Pro", 
                color = grey90k,
                position = position_nudge(x = 0.1),
                vjust = -0.5)+
      si_style_ygrid() + 
      facet_wrap(~snu1,
                 scales = "free_y")+
      scale_y_continuous(labels = comma, expand = expansion(mult = c(0.05, 0.1)))+
      scale_x_continuous(expand = c(0,.75),
                         breaks = c(2021,2022,2023),  
                         label = c("FY21", "FY22", "FY23")
      )+
      #coord_cartesian(expand = T) + 
      theme(plot.subtitle = ggtext::element_markdown())+
      labs(x = NULL, y = NULL, 
           caption = glue::glue("{metadata$caption} |
                                  Known Status = HIV-infected + HIV-uninfected"),
           title = "USAID PROPORTION OF KNOWN STATUS IN PMTCT_FO")
    
    si_save("Images/ZMB_USAID_PMTCT_FO_FY23.png")
    
  
    
    # SPINDOWN ============================================================================