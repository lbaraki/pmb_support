# PROJECT: PPIR Quarterly Data Review
# PURPOSE: Munge and Analysis of TX_MMD & VLC
# AUTHOR:  Lemlem Baraki | SI
# REF ID:  e9da1cfc
# LICENSE: MIT
# DATE:   2024-06-12
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
  #library(gtExtras)
  library(glue)
  library(selfdestructin5)
  library(cascade)

    
  # SI specific paths/functions  
    load_secrets()
    merdata <- file.path(glamr::si_path("path_msd"))
    file_path <- return_latest(folderpath = merdata, pattern = "OU_IM_FY22-24")
    
      
  # Grab metadata
   metadata <- get_metadata(file_path)
  
  # REF ID for plots
    ref_id <- "e9da1cfc"
    
  # Functions  
    clean_number <- function(x, digits = 0){
      dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                       x >= 1e6 ~ glue("{round(x/1e6, 2)}M"),
                       x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                       TRUE ~ glue("{x}"))
    }
    

# LOAD DATA ============================================================================  

    df <- read_psd(file_path) %>% 
      clean_agency() %>% 
      filter(funding_agency == "USAID",
             operatingunit %ni% c("Ukraine"),
             fiscal_year %in% c(2023:2024),
             
             #operatingunit == "Nigeria"
             ) #%>% 
      #resolve_knownissues()
    
    names(df)
    
    #ind_sel <- c("Nigeria")

# MUNGE ============================================================================
  
  #Clinical Cascade 
    #https://usaid-oha-si.github.io/cascade/index.html
    cascade::plot_name #peds - 4 
    return_cascade(df, 4) #%>% View()
    return_cascade_plot(df %>% filter(!(operatingunit =="Nigeria")),
                                      #ageasentered %in% c("<01","01-04","05-09",
                                       #                   "10-14", "15-19")) %>% 
                        export = F) + 
      plot_annotation(caption = glue("<span style= 'color:{grey60k}'>Note:
                                     Nigeria excluded due to missing data</span>"),
                      theme = theme(plot.caption = ggtext::element_markdown()))
    
    #si_save("Graphics/FY24Q2_ClinicalCascade_Peds.png")
    si_save(glue("Graphics/{metadata$curr_pd}_Cascade_Peds_Nigeria.png"), scale = 1.25)
    si_save(glue("Graphics/{metadata$curr_pd}_Cascade_Peds_AllOUs.png"), scale = 1.25) #w/o Nigeria
    
  #Treatment Table
    #mdb_df_tx <- make_mdb_tx_df(df)
    #mdb_tbl <- reshape_mdb_tx_df(mdb_df_tx, metadata$curr_pd)
    
  #Pediatric TX_MMD for USAID 
    mmd_df <- df %>% 
      filter(
        funding_agency == "USAID",
        fiscal_year %in% c(2023:2024),
        indicator %in% c("TX_CURR"),
        #ageasentered %in% c("10-14", "15-19"),
        trendscoarse == "<15",
        standardizeddisaggregate %in% c(
          "Age/Sex/ARVDispense/HIVStatus"
        )
      ) %>% 
      gophr::clean_indicator() %>% 
      group_by(indicator, otherdisaggregate, fiscal_year) %>% #operatingunit
      summarise(across(starts_with("qtr"), sum, na.rm = TRUE), 
                .groups = "drop") %>%
      reshape_msd(include_type = FALSE) %>%
      pivot_wider(
        names_from = otherdisaggregate,
        names_glue = "{tolower(otherdisaggregate)}"
      ) %>% 
      arrange(desc(period)) %>%
      #group_by(operatingunit, period) %>% 
      rename(
       `3-5 months on ARVs` = `arv dispensing quantity - 3 to 5 months`,
        `6+ months on ARVs` = `arv dispensing quantity - 6 or more months`,
        `<3 months on ARVs` = `arv dispensing quantity - less than 3 months`
      ) %>% 
      mutate(
        total =  `3-5 months on ARVs` +  `6+ months on ARVs` +  `<3 months on ARVs`,
        percent_three = `<3 months on ARVs`/total,
        percent_middle = `3-5 months on ARVs`/total,
        percent_six =  `6+ months on ARVs`/total)
      
    #%>% 
    #ungroup()
      
  
# VIZ ============================================================================

  #Proportion of ARV's dispensed to children (MMD Uptake)
      #ex) 6 months or more of ART treatment dispensed 
      #percentage: TX_MMD months/total TX_MMD
    
    mmd_df %>% 
      ggplot(aes(x = period, group = 1
                 )) + 
      geom_line(aes(y = `3-5 months on ARVs`), color = "#287c6f", linewidth = 0.8) + 
      geom_point(aes(y = `3-5 months on ARVs`), color = "#287c6f") + 
      geom_text(data = . %>% slice(head(seq(n()), 2)),
        aes(y = `3-5 months on ARVs`, 
                    label = comma(`3-5 months on ARVs`)),
                size = 10/.pt,
                family = "Source Sans Pro SemiBold", vjust = 2.5) + 
      
      geom_line(aes(y = `6+ months on ARVs`), color = "#2057a7", linewidth = 0.8) + 
      geom_point(aes(y = `6+ months on ARVs`), color = "#2057a7") + 
      geom_text(data = . %>% slice(head(seq(n()), 2)),
        aes(y = `6+ months on ARVs`,
                    label = comma(`6+ months on ARVs`)), size = 10/.pt,
                family = "Source Sans Pro SemiBold", vjust = -2.5) + 
      
      geom_line(aes(y = `<3 months on ARVs`), color = "#e07653", linewidth = 0.8) + 
      geom_point(aes(y = `<3 months on ARVs`), color = "#e07653") + 
      geom_text(data = . %>% slice(head(seq(n()), 2)),
                aes(y = `<3 months on ARVs`,
      label = comma(`<3 months on ARVs`)), size = 10/.pt,
                family = "Source Sans Pro SemiBold", vjust = 2.75) + 
      
      si_style_ygrid(facet_space = 0.5) + 
      scale_y_continuous(labels = scales::number) + #scales::number
      theme(plot.subtitle = ggtext::element_markdown())+
      labs(x = NULL, y = NULL, 
           subtitle = glue::glue("<span style = 'color:#e07653'><3 months </span> |
            <span style = 'color:#287c6f'>3-5 months </span> |
            <span style = 'color:#2057a7'>6+ months </span> "),
           caption =glue::glue("{metadata$caption} | Ref id:{ref_id}"),
         title = "Pediatric TX_MMD Trends from FY23 - FY24")
    
    si_save("Graphics/FY24Q2_TX_MMD_Trends_USAID_Peds.png")
    si_save("Graphics/FY24Q2_TX_MMD_Trends_USAID_Peds_V2.png")
    si_save("Graphics/FY24Q2_TX_MMD_Trends_USAID_Peds_V3.png")
    si_save("Graphics/FY24Q2_TX_MMD_Trends_USAID_Peds_Q1vsQ2.png")
    
    mmd_df %>% 
      ggplot(aes(x = period, group = 1
      )) + 
      geom_line(aes(y = percent_middle), color = "#287c6f", linewidth = 0.8) + 
      geom_point(aes(y = percent_middle), color = "#287c6f") + 
      geom_text(data = . %>% slice(head(seq(n()), 2)),
                aes(y = percent_middle, #`3-5 months on ARVs`, 
                    label = percent(percent_middle,1)),
                size = 10/.pt,
                family = "Source Sans Pro SemiBold",
                vjust = 2.5) + 
      
      geom_line(aes(y = percent_six), color = "#2057a7", linewidth = 0.8) + 
      geom_point(aes(y = percent_six), color = "#2057a7") + 
      geom_text(data = . %>% slice(head(seq(n()), 2)),
                aes(y = percent_six, #`6+ months on ARVs`,
                    label = percent(percent_six, 1)),
                size = 10/.pt,
                family = "Source Sans Pro SemiBold", vjust = -2.5) + 
      
      geom_line(aes(y = percent_three), color = "#e07653", linewidth = 0.8) + 
      geom_point(aes(y = percent_three), color = "#e07653") + 
      geom_text(data = . %>% slice(head(seq(n()), 2)),
                aes(y = percent_three,#`<3 months on ARVs`,
                    label = percent(percent_three, 1)),
                size = 10/.pt,
                family = "Source Sans Pro SemiBold", vjust = 2.75) + 
      
      si_style_ygrid(facet_space = 0.5) + 
      scale_y_continuous(
        labels = scales::percent
        ) + #scales::number
      theme(plot.title = ggtext::element_markdown(),
            plot.subtitle = ggtext::element_markdown())+
      labs(x = NULL, y = NULL, 
           subtitle = glue::glue("<span style = 'color:#e07653'><3 months </span> |
            <span style = 'color:#287c6f'>3-5 months </span> |
            <span style = 'color:#2057a7'>6+ months </span> "),
           caption =glue::glue("{metadata$caption} | Ref id:{ref_id}"),
           title = "Pediatric TX_MMD Trends from FY23 - FY24"
           #title = glue("PEDS <span style = 'color:#2057a7'>6+MMD</span> INCREASED BY <span style = 'color:#2057a7'>3%</span>, BRINGING
            #               3+MMD TO 77% IN {metadata$curr_pd}")
           )
    
    si_save("Graphics/FY24Q2_TX_MMD_Percentage_USAID_Peds.png")
    si_save("Graphics/FY24Q2_TX_MMD_Percentage_USAID_Peds_v2.png")
    
      

# SPINDOWN ============================================================================

