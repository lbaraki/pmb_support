# PROJECT: PPIR Quarterly Data Review
# PURPOSE: Munge and Analysis of VLC/VLS
# AUTHOR:  Lemlem Baraki | SI
# REF ID:  6b0070eb
# LICENSE: MIT
# DATE:   2024-06-13
# NOTES: https://github.com/tessam30/Here-it-goes-again/blob/4fdbe5175b07f96fd710c4d4ae2d545e36d2dcdf/Scripts/04_viral_load_analysis.R  

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

    
  # SI specific paths/functions  
  load_secrets()
  merdata <- file.path(glamr::si_path("path_msd"))
  file_path <- return_latest(folderpath = merdata, pattern = "OU_IM_FY22-24")

      
  # Grab metadata
   metadata <- get_metadata(file_path)
  
  # REF ID for plots
    ref_id <- "6b0070eb"
    
  # Functions 
      #Create a wide VLC data frame 
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
            vls_adj = tx_pvls / tx_curr_lag2
          ) %>% 
          ungroup()
        return(df)
      }  
  

# LOAD DATA ============================================================================  

    df_msd <- read_psd(file_path) %>% 
      clean_agency() #%>% 
      #filter(fiscal_year %in% c(2022:2024), funding_agency == "USAID")

# MUNGE ============================================================================
  
  #VLC/VLS Tables
    df_vl_peds <- df_msd %>% 
      filter(funding_agency =="USAID",
             trendscoarse == "<15",
             #operatingunit %ni% c("Uganda", "Malawi", "Democratic Republic of the Congo", "South Sudan")
             ) %>% 
      create_vl_df(trendscoarse, operatingunit
        #ageasentered
        ) %>% 
        filter(str_detect(period, "20", negate = T),
               period %in% c("FY23Q2","FY23Q3","FY23Q4",
                             "FY24Q1","FY24Q2")
               ) #%>% View()
      
      df_vl_fine <- df_msd %>% 
        filter(funding_agency =="USAID",
               #ageasentered %in% c("01-04", "05-09","10-14")
               ) %>% 
        create_vl_df(ageasentered) %>% 
        filter(str_detect(period, "20", negate = T),
               period %in% c("FY23Q1","FY23Q2","FY23Q3","FY23Q4",
                             "FY24Q1","FY24Q2")
        ) #%>% View()
  
# VIZ ============================================================================

  #VLC Viz
      num_pds <- length(unique(df_vl_peds$period))
      
      top <- 
        df_vl_peds %>% 
          #filter(ageasentered %in% c("01-04", "05-09","10-14")) %>% 
        ggplot(aes(x = period, group = 1)) +
        #geom_line(aes(y = vls), color = burnt_sienna) +
        #geom_point(aes(y = vls), shape = 21, fill = burnt_sienna, size = 3,
         #          color = "white") +
        geom_line(aes(y = vlc), color = denim) +
        geom_point(aes(y = vlc), shape = 21, fill = denim, size = 3,
                   color = "white") +
        geom_text(aes(y = vlc, label = percent(vlc, 1)), size = 9/.pt,
                  family = "Source Sans Pro", color = denim, 
                  vjust = -1) +
        #geom_text(aes(y = vls, label = percent(vls, 1)), size = 9/.pt,
         #         family = "Source Sans Pro", color = burnt_sienna, 
          #        vjust = -1) +
        #annotate("text", x = num_pds + .5,
         #        y = .97, #move around as needed
          #       label = "Viral Load\nSuppression",
           #      color = burnt_sienna, size = 10/.pt,
            #     hjust = 0.1, 
             #    family = "Source Sans Pro") +
          
        #annotate("text", x = num_pds + .5,
         #        y = .9, #move up and down as needed (.7 to .9)
          #       label = "Viral Load\nCoverage",
           #      color = denim, size = 10/.pt,
            #     hjust = 0.1, 
             #    family = "Source Sans Pro") +
          
          #facet_wrap(~ageasentered)+
        si_style_nolines() +
        expand_limits(x = c(1, num_pds+2), y = c(0.7,1.05)) +
        theme(plot.title = ggtext::element_markdown(),
              axis.text.y = element_blank(), 
              axis.text.x = element_blank()) +
        labs(x = NULL, y = NULL,
             #title = glue("PEDS <span style = 'color:{denim}'>VIRAL LOAD COVERAGE</span> INCREASED BY 9% -
              #             FROM <span style = 'color:{denim}'>82%</span> IN FY24Q1 TO \n
               #           <span style = 'color:{denim}'>91%</span> IN {metadata$curr_pd}"))
             title = glue("PEDS <span style = 'color:{denim}'>VIRAL LOAD COVERAGE</span> INCREASED BY 9% -
                           FROM <span style = 'color:{denim}'>82%</span> IN FY24Q1 TO \n
                          <span style = 'color:{denim}'>91%</span> IN {metadata$curr_pd}"))
        
        
        bottom <- 
        df_vl_peds %>% 
          ggplot(aes(x = period)) +
          geom_col(aes(y = tx_curr_lag2), fill = grey10k) +
          geom_col(aes(y = tx_pvls_d), fill = denim) +
          si_style_ygrid() +
          scale_y_continuous(labels = comma) +
          expand_limits(x = c(1, num_pds+2)) +
          labs(x = NULL, y = NULL) +
          annotate("segment", x = num_pds + .5, xend = num_pds + .5,
                   y = 185000, yend = 250000, #change as needed --> min/max
                   color = grey70k) +
          annotate("text", x = num_pds + .65,
                   y = 220000, #change as needed
                   label = "Coverage gap", 
                   hjust = 0,
                   size = 10/.pt,
                   family = "Source Sans Pro", color = grey70k) #+
          #annotate("text", x = num_pds+1, y = 250000, #change TX_CURR_lag --> ~250k max
           #        label = "TX_CURR_LAG2", 
            #       size = 8/.pt,
             #      family = "Source Sans Pro", color = grey50k) +
          #annotate("text", x = num_pds+1, y = 185000, #change TX_PVLS_D --> ~190k
           #        label = "TX_PVLS_D", 
            #       size = 8/.pt,
             #      family = "Source Sans Pro", color = denim)
       
        
        #Linked  
        top / bottom + plot_layout(heights = c(1, 3)) +
          #theme(plot.title = ggtext::element_markdown()) +
          #labs(title = "***") + 
          plot_annotation(#title = glue("PEDIATRIC <span style = 'color:{scooter}'VIRAL LOAD COVERAGE</span> INCREASED BY 9% -
                           #            FROM 82% IN FY24Q1 TO 91% IN {metadata$curr_pd}"),
                          caption = metadata$caption) &
          theme(plot.tag = element_text(family = "Source Sans Pro"))
        
        si_save("Graphics/VL_summary_peds_FY24Q2.png")
        #si_save("Graphics/VL_summary_eds_2024.svg")
        
        

# VL by Age/Sex -----------------------------------------------------------

        df_vl_agebands<- 
          df_msd %>% 
          filter(funding_agency == "USAID",
                 indicator %in% c("TX_CURR","TX_CURR_Lag2", "TX_PVLS"),
                 standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age/Sex/Indication/HIVStatus"),
                 ageasentered != "Unknown Age") %>% 
          mutate(ageasentered = case_when(#trendscoarse == "<15" ~ trendscoarse,
                                          ageasentered %in% c("50-54","55-59", "60-64", "65+") ~ "50+",
                                          TRUE ~ ageasentered)) %>% 
          clean_indicator() %>% 
          group_by(indicator, ageasentered, fiscal_year) %>% 
          summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
          reshape_msd(include_type = FALSE) %>% 
          pivot_wider(names_from = indicator,
                      names_glue = "{tolower(indicator)}") %>% 
          filter(ageasentered %in% c("01-04","05-09","10-14"),
                 period %in% c("FY22Q3", "FY23Q4",
                               "FY23Q1","FY23Q2","FY23Q3","FY23Q4",
                               "FY24Q1","FY24Q2")) %>% 
          #View()
          arrange(ageasentered, period) %>% 
          group_by(ageasentered) %>% 
          mutate(tx_curr_lag2 = lag(tx_curr, n = 2),
                 vlc = tx_pvls_d/tx_curr_lag2,
                 vls = tx_pvls / tx_pvls_d,
                 vls_adj = tx_pvls /tx_curr_lag2) %>% 
          ungroup()
          
        
        df_vl_agebands %>% 
          filter(period %ni% c("FY22Q3", "FY22Q4", "FY23Q1")) %>% 
          ggplot(aes(x = period, group = 1)) +
          #geom_line(aes(y = vls), color = burnt_sienna) +
          #geom_point(aes(y = vls), shape = 21, fill = burnt_sienna, size = 3,
           #          color = "white") +
          geom_line(aes(y = vlc), color = denim, linewidth =0.8 ) +
          geom_point(aes(y = vlc), shape = 21, fill = denim, size = 3,
                     color = "white") +
          geom_text(aes(y = vlc, label = percent(vlc, 1)), size = 11/.pt,
                    family = "Source Sans Pro", color = denim, 
                    vjust = -1) +
          #geom_text(aes(y = vls, label = percent(vls, 1)), size = 9/.pt,
           #         family = "Source Sans Pro", color = burnt_sienna, 
            #        vjust = -1) +
          facet_wrap(~ageasentered) +
          #scale_x_discrete(breaks = every_nth(n = 4)) +
          scale_x_discrete(labels = c("FY23Q2", "FY23Q3",
                                      "FY23Q4",
                                      "FY24Q1","FY24Q2")) +
          si_style_nolines(facet_space = 0.5) +
          expand_limits(x = c(1, 10),
                        y = c(0.7,1.05)
                        ) + #default .7 - 1.05
          theme(plot.title = ggtext::element_markdown(),
            axis.text.y = element_blank()) +
          labs(x = NULL, y = NULL,
               title = glue("PEDIATRIC <span style = 'color:{denim}'>VIRAL LOAD COVERAGE</span> TRENDS"), 
               caption = glue("{metadata$caption}"))
        
        si_save("Graphics/PEDS_vls_vlc_by_fineage.png")  
  

# SPINDOWN ============================================================================

