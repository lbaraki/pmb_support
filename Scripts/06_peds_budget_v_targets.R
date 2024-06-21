# PROJECT: PAC Review of COP23/ROP24
# PURPOSE: Munge and Analysis of Peds Budget/Targets
# AUTHOR:  Lemlem Baraki | SI
# REF ID:  9add057c
# LICENSE: MIT
# DATE:   2024-03-21
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
  library(readxl)
  library(glue)
  library(gt)
  library(gtExtras)
    
  
  # REF ID for plots
    ref_id <- "9add057c"
    
  # Functions  
    clean_number <- function(x, digits = 0){
      dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                       x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                       x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                       TRUE ~ glue("{x}"))
    }
# LOAD DATA ============================================================================  
  
  #Sheet 1: Overall Peds Budget for HTS & C/T
  cop_dossier <- read_excel("Data/COP_PMB_Budget&Targets.xlsx",1)
    
  #Sheet 2: HTS Budget for Each Country (2024:2025)
  #Sheet 3: C/T Budget for Each Country (2024:2025)
  hts_budget <- read_excel("Data/COP_PMB_Budget&Targets.xlsx",2)
  ct_budget <- read_excel("Data/COP_PMB_Budget&Targets.xlsx",3)
  
  #Sheet 4: TX_CURR target for each country (2023:2025)
  tx_curr <- read_excel("Data/COP_PMB_Budget&Targets.xlsx",4)
  
  #Sheet 5: HTS_POS target for each country (2023: 2025) 
    #PEPFAR
  hts_pos <- read_excel("Data/COP_PMB_Budget&Targets.xlsx",5)
    #USAID
  hts_pos_agency <- read_excel("Data/COP_PMB_Budget&Targets.xlsx",6)
  
  hts_agency <- read_excel("Data/COP_PMB_Budget&Targets.xlsx",7)
  
  txcurr_agency <- read_excel("Data/COP_PMB_Budget&Targets.xlsx",8)

# MUNGE ============================================================================
  
  #Has there been a signficant shift in pediatrics budgets btwn FY24-FY25? 
    #C&T: encompasses TX_NEW, TX_CURR
    #HTS: encompasses HTS_TST, HTS_POS, etc. 
  
  cop_dossier <- cop_dossier %>% 
    pivot_wider(names_from = `Fiscal Year`,
                values_from = `Total Planned Funding`) %>% 
    mutate(
      percent_diff = ( `2025` - `2024`)/( `2024`)*100) #23% decrease in HTS funding 
  
  glimpse(cop_dossier)
  
# VIZ ============================================================================

  #Q1: Change in budget levels across pediatric program areas
    #group by program area
  cop_dossier %>%
    mutate(`Fiscal Year` = as.integer(`Fiscal Year`)) 
  
  cop_dossier%>% 
    mutate(funds = clean_number(`Total Planned Funding`),
           program = `Program Area`) %>% 
    #relocate(`Fiscal Year`, `Total Planned Funding`, funds, program, `Program Area`) %>% 
    #unite("value_label", funds:program, sep = " ", na.rm = TRUE) %>% 
    #mutate(value_label = case_when(
     # `Fiscal Year` == max(`Fiscal Year`) ~ paste(comma(`Total Planned Funding`), `Program Area`),
      #TRUE ~ comma(`Total Planned Funding`))) %>% 
    ggplot(aes(x= `Fiscal Year`, y =`Total Planned Funding`
               #,group = `Program Area`
               )) + 
    geom_line(aes(color = `Program Area`))+ #color = glitr::denim 
    geom_point(aes(color = `Program Area`), size = 2.5) + #color = glitr::denim
    ggrepel::geom_text_repel(aes(label = funds),
                             vjust = 1, force = 4) + 
    si_style_xline() + 
    scale_color_si(palette = "old_rose", discrete = T) + 
    facet_wrap(~`Program Area`, scale = "free_y") + 
    scale_x_continuous(breaks = 2024:2025) + 
    expand_limits(y = c(1000000,100000000)) + 
    #scale_y_continuous(breaks = c(5000000,50000000,1000000000)) + 
    theme(axis.text.y = element_blank(),
          plot.title = ggtext::element_markdown(),
          plot.caption= ggtext::element_markdown(),
          #plot.subtitle = ggtext::element_markdown(),
          legend.position = "none") + 
    labs(
      y = NULL, x = NULL, 
      #title = "PEDIATRIC BUDGET SHIFTS FROM FY24-FY25", 
      title = glue("IN FY25, PEDS <span style = 'color:{scooter}'>HTS FUNDING</span> EXPERIENCED A <span style = 'color:{scooter}'>23% DECREASE</span>, WHILE 
      <span style = 'color:{old_rose}'>C&T FUNDING</span> EXPERIENCED A <span style = 'color:{old_rose}'>4% INCREASE</span> \n"),
      subtitle = "Global Alliance OU's: Angola, DRC, Mozambique, Nigeria, Tanzania, Zambia, Zimbabwe",
      caption = glue("Source: Financial & MER Integrated Analytics | [{lubridate::today()}]")
    )
  
  si_save("Images/USAID_peds_program_budget_change_line.png")

# Q2: HTS Program Budget breakdown by OU ----------------------------------
      #x-axis: values, y-xis: value, facet: OU
      #fct_reorder 
  hts_budget %>% 
    pivot_longer(cols = `2024`: `2025`, names_to = "FY") %>%
    filter(`Operating Unit` %ni% c("Tanzania", "Zimbabwe", "Total")) %>% 
    mutate(`Operating Unit` = case_when(
      `Operating Unit` %in% c("Democratic Republic of the Congo") ~ "DRC",
      TRUE ~ `Operating Unit`)) %>% 
    arrange(desc(value)) %>% 
    ggplot(aes(x = FY, y = value, group = "a")) + 
    geom_line() + 
    #geom_area(aes(fill = `Operating Unit`), alpha = 0.75) +
    geom_area(fill = glitr::denim, alpha = 0.75) +
    ggrepel::geom_text_repel(aes(label = clean_number(value)),
                             vjust = 1, force = 1)+ 
    facet_wrap(~factor(`Operating Unit`, c("Angola","DRC","Zambia","Nigeria","Mozambique")),
                       ncol= 3) +
    #scale_fill_si(palette = "old_rose", discrete = T) + 
    scale_x_discrete()+ 
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()))+
    si_style_ygrid() + 
    theme(legend.position = "none") +
    labs(title = "PEDS HTS PROGRAM BUDGET SHIFTS BY OU",
         caption = "Excluding Cameroon, Côte d'Ivoire, Kenya, South Africa, Tanzania, Uganda, Zimbabwe due to missing data |
      Source: Financial & MER Integrated Analytics")
    
    #can also use tables for this info and do % change
  hts_budget %>% 
    #ct_budget %>% 
    filter(`Operating Unit` != "Total") %>% 
    #mutate(`Operating Unit` = case_when(
     # `Operating Unit` %in% c("Democratic Republic of the Congo") ~ "DRC",
      #TRUE ~ `Operating Unit`)) %>% 
  mutate(delta = `2025`-`2024`,
         pct_delta = (`2025`-`2024`)/(`2024`)) %>%
    mutate(decline_shp = ifelse(pct_delta < 0,"\u25Bc", "\u25B2")) %>%
    arrange(pct_delta) %>%
    gt(groupname_col = "Funding Agency") %>% 
    fmt_currency(columns = 4:6, 
               decimals = 0) %>% 
    fmt_percent(columns = 7, decimals = 0) %>% #make sure the columns actually match up
    gt_hulk_col_numeric(7, trim = TRUE, ) %>% 
    cols_label(Program = "",
               pct_delta = "% change",
               decline_shp = "") %>% 
    grand_summary_rows(
      columns = 4:6,
      fns = list(Total = ~sum(., na.rm = TRUE)),
      formatter = fmt_currency,
      decimals = 0) %>% 
    tab_source_note(
      source_note = glue("Source: Financial & MER Integrated Analytics | [{lubridate::today()}]")) %>% 
    tab_options(
      source_notes.font.size = px(10)) %>% 
    tab_header(
      title = glue("BUDGET CHANGE SUMMARY: FY24 TO FY25"),
    ) %>% 
    gt_theme_nytimes() %>%
    gtsave_extra("Images/USAID_peds_hts_budget_change_summary_table.png")
    
    

# Q3: HTS target breakdown by OU ------------------------------------------
  #How is this budget decrease reflected in HTS targets for these OU's? 
    #fct_reorder the OU's by their value for each FY (when categorical aka OU is mapped to size/value)
    
  hts_pos %>% 
    pivot_longer(cols = `2023`: `2025`, names_to = "FY") %>%
    mutate(`Operating Unit` = case_when(
      `Operating Unit` %in% c("Democratic Republic of the Congo") ~ "DRC",
      TRUE ~ `Operating Unit`)) %>% 
    #filter(FY %ni% c(2023)) %>% 
    #mutate(FY = as.factor(FY),
     #      country_order = reorder_within(`Operating Unit`, value, FY)) %>% 
    mutate(FY_color = ifelse(FY == "2023", 
                             glitr::scooter,
                             glitr::grey60k)) %>% 
    #Create a column called order to store the factor order
    arrange(FY, value) %>% 
    mutate(order = row_number()) %>% 
    #wrap in curly brackets 
    {
    #use order for x-axis
    ggplot(.,aes(y = value, x = order, fill = FY_color)) + 
    geom_col() + 
    coord_flip() + 
    facet_wrap(~FY, nrow = 1, scales = "free_y") + 
    scale_fill_identity() + 
    si_style_xgrid() + 
        scale_x_continuous(
          breaks = .$order, 
          labels = .$`Operating Unit`,
          expand = c(0,.4) 
        ) + 
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
    theme(legend.position = "none") +
    labs(x = NULL, y = NULL,
         title = "HTS_TST_POS RESULTS & TARGETS",
         caption = "Source: Financial & MER Integrated Analytics")
    }
  
  
  hts_pos_agency %>% 
    pivot_longer(cols = `2024`: `2025`, names_to = "FY") %>%
    mutate(`Operating Unit` = case_when(
      `Operating Unit` %in% c("Democratic Republic of the Congo") ~ "DRC",
      TRUE ~ `Operating Unit`)) %>% 
    mutate(FY_color = ifelse(FY == "2023", 
                             glitr::scooter,
                             glitr::grey60k)) %>% 
    #Create a column called order to store the factor order
    arrange(FY, value) %>% 
    mutate(order = row_number()) %>% 
    #wrap in curly brackets 
    {
      #use order for x-axis
      ggplot(.,aes(y = value, x = order, fill = FY_color)) + 
        geom_col() + 
        coord_flip() + 
        geom_text(aes(label = comma(value)), size = 11/.pt,
                  hjust = .5,
                  family = "Source Sans Pro SemiBold")+ 
        facet_wrap(~FY, nrow = 1, scales = "free_y") + 
        scale_fill_identity() + 
        si_style_xgrid() + 
        scale_x_continuous(
          breaks = .$order, 
          labels = .$`Operating Unit`,
          expand = c(0,.4) 
        ) + 
        scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
        theme(legend.position = "none") +
        labs(x = NULL, y = NULL,
             title = "HTS_TST_POS TARGETS FOR <15",
             caption = "Source: Financial & MER Integrated Analytics")
    }

  
  tx_curr %>% 
    pivot_longer(cols = `2023`: `2025`, names_to = "FY") %>%
    mutate(`Operating unit` = case_when(
      `Operating unit` %in% c("Democratic Republic of the Congo") ~ "DRC",
      TRUE ~ `Operating unit`)) %>% 
    #filter(FY %ni% c(2023)) %>% 
    #mutate(FY = as.factor(FY),
    #      country_order = reorder_within(`Operating Unit`, value, FY)) %>% 
    mutate(FY_color = ifelse(FY == "2023", 
                             glitr::scooter,
                             glitr::grey60k)) %>% 
    #Create a column called order to store the factor order
    arrange(FY, value) %>% 
    mutate(order = row_number()) %>% 
    #wrap in curly brackets 
    {
      #use order for x-axis
      ggplot(.,aes(y = value, x = order, fill = FY_color)) + 
        geom_col() + 
        coord_flip() + 
        facet_wrap(~FY, nrow = 1, scales = "free_y") + 
        scale_fill_identity() + 
        si_style_xgrid() + 
        scale_x_continuous(
          breaks = .$order, 
          labels = .$`Operating unit`,
          expand = c(0,.4) 
        ) + 
        scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
        theme(legend.position = "none") +
        labs(x = NULL, y = NULL,
             title = "TX_CURR RESULTS & TARGETS",
             caption = "Source: Financial & MER Integrated Analytics")
    }
    
  
  #GT TABLE VERSION - simpler and shows % change
  
  htspos_save<- hts_pos_agency %>%
    pivot_longer(cols = `2024`: `2025`, names_to = "FY") %>%
    group_by(`Coarse Age`,  Indicator, FY) %>% 
    spread(FY, value) %>% 
    mutate(delta = `2025`-`2024`,
           pct_delta = (`2025`-`2024`)/(`2024`)) %>%
    mutate(decline_shp = ifelse(pct_delta < 0,"\u25Bc", "\u25B2"))
 
 hts_save <- hts_agency %>%
   pivot_longer(cols = `2024`: `2025`, names_to = "FY") %>%
   group_by(`Coarse Age`,  Indicator, FY) %>% 
   spread(FY, value) %>% 
   mutate(delta = `2025`-`2024`,
          pct_delta = (`2025`-`2024`)/(`2024`)) %>%
   mutate(decline_shp = ifelse(pct_delta < 0,"\u25Bc", "\u25B2"))
 
  txcurr_save<- tx_curr %>%
   pivot_longer(cols = `2023`: `2025`, names_to = "FY") %>%
   filter(FY !="2023") %>% 
   group_by(`Coarse Age`,  Indicator, FY) %>% 
   spread(FY, value) %>% 
   mutate(delta = `2025`-`2024`,
          pct_delta = (`2025`-`2024`)/(`2024`)) %>%
   mutate(decline_shp = ifelse(pct_delta < 0,"\u25Bc", "\u25B2")) 
  
  txcurr_ag<- txcurr_agency %>%
    pivot_longer(cols = `2024`: `2025`, names_to = "FY") %>%
    #filter(FY !="2023") %>% 
    group_by(`Coarse Age`,  Indicator, FY) %>% 
    spread(FY, value) %>% 
    mutate(delta = `2025`-`2024`,
           pct_delta = (`2025`-`2024`)/(`2024`)) %>%
    mutate(decline_shp = ifelse(pct_delta < 0,"\u25Bc", "\u25B2")) 
  
  #htspos_save %>%
   hts_save %>% 
   filter(`Operating Unit` !="Total") %>%
   arrange(pct_delta) %>% 
   #mutate(`Operating Unit` = case_when(`Operating Unit` %in% c("Democratic Republic of the Congo") ~ "DRC",
     #TRUE ~ `Operating Unit`)) %>% 
 gt(groupname_col = "Coarse Age") %>% 
    fmt_number(columns = 3:6, 
               decimals = 0) %>% 
    fmt_percent(columns = 7, decimals = 0) %>% #make sure the columns actually match up
    gt_hulk_col_numeric(7, trim = TRUE, ) %>% 
    cols_label(Indicator = "",
               pct_delta = "% change",
               decline_shp = "") %>% 
   grand_summary_rows(
     columns = 4:6,
     fns = list(Total = ~sum(., na.rm = TRUE)),
     formatter = fmt_number,
     decimals = 0) %>% 
    tab_source_note(
      source_note = glue("Source: Target Seting Tool Dossier | [{lubridate::today()}]")) %>% 
  tab_options(
    source_notes.font.size = px(10)) %>% 
  tab_header(
    title = glue("TARGET CHANGE SUMMARY: FY24 TO FY25"),
  ) %>% 
  gt_theme_nytimes() %>%
   gtsave_extra("Images/USAID_peds_hts_target_change_summary_table.png")
 
 txcurr_ag %>%
   arrange(pct_delta) %>% 
   #mutate(`Operating Unit` = case_when(
   # `Operating Unit` %in% c("Democratic Republic of the Congo") ~ "DRC",
   #TRUE ~ `Operating Unit`)) %>% 
   gt(groupname_col = "Coarse Age") %>% 
   fmt_number(columns = 3:6, 
              decimals = 0) %>% 
   fmt_percent(columns = 7, decimals = 0) %>% #make sure the columns actually match up
   gt_hulk_col_numeric(7, trim = TRUE, ) %>% 
   cols_label(Indicator = "",
              pct_delta = "% change",
              decline_shp = "") %>% 
   tab_source_note(
     source_note = glue("Source: Target Setting Tool Dossier | [{lubridate::today()}]")) %>% 
   tab_options(
     source_notes.font.size = px(10)) %>% 
   tab_header(
     title = glue("TARGET CHANGE SUMMARY: FY24 TO FY25"),
   ) %>% 
   gt_theme_nytimes() %>% 
   gtsave_extra("Images/USAID_peds_txcurr_target_change_summary_table.png")
  
    
  
  #Bar chart w/ sum of indicators
    #hts_pos_agency %>% 
     # pivot_longer(cols = `2024`: `2025`, names_to = "FY") %>%
      #ggplot(aes(x = factor(FY), y = value, fill = `Operating Unit`)) +
      #geom_col() 
      #facet_wrap(~indicator, scales = "free_y", nrow = 1)
# SPINDOWN ============================================================================

  
  