# PROJECT: Aging Out Analysis
# PURPOSE: Munge and Analysis of TX_NEW, NET_NEW
# AUTHOR:  Lemlem Baraki | SI
# REF ID:  ecdd279b
# LICENSE: MIT
# DATE:   2024-04-17
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
library(glue)



# SI specific paths/functions  
load_secrets()
merdata <- file.path(glamr::si_path("path_msd"))
file_path <- return_latest(folderpath = merdata, pattern = "OU_IM_FY22-25")

# Grab metadata
meta <- get_metadata(file_path)

# REF ID for plots
ref_id <- "ecdd279b"

# Functions  
every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}  

clean_number <- function(x, digits = 0){
  dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                   x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                   x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                   TRUE ~ glue("{x}"))
}

# LOAD DATA ============================================================================  

df_msd <- read_psd(file_path) %>% 
  clean_agency()

names(df)

# MUNGE ============================================================================

#TX_NEW and NET_NEW 
#<15
df_txnew <-  df_msd %>% 
  filter(funding_agency == "USAID",
         trendscoarse == "<15",
         #ageasentered == "15-19",
         indicator %in% c("TX_NEW", "TX_NET_NEW"),
         standardizeddisaggregate %in% c("Age/Sex/HIVStatus",
                                         "Age Aggregated/Sex/CD4/HIVStatus",
                                         "Age/Sex/CD4/HIVStatus")
  ) %>% 
  group_by(fiscal_year, indicator, operatingunit, standardizeddisaggregate) %>% #trendscoarse/ageasentered
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE),
            .groups = "drop") %>%
  reshape_msd(include_type = FALSE) %>% 
  pivot_wider(
    names_from = indicator,
    names_glue = "{tolower(indicator)}") %>%
  arrange(desc(period)) #%>% 
#prinf()

#15-19
#note: TX_NEW is now under "Age/Sex/CD4/HIVStatus" as of Q1
df_txnew <-  df_msd %>% 
  filter(funding_agency == "USAID",
         #trendscoarse == "<15",
         ageasentered == "15-19",
         indicator %in% c("TX_NEW", "TX_NET_NEW"),
         standardizeddisaggregate %in% c("Age/Sex/HIVStatus",
                                         "Age Aggregated/Sex/CD4/HIVStatus",
                                         "Age/Sex/CD4/HIVStatus")
  ) %>% 
  group_by(fiscal_year, indicator, operatingunit) %>% #trendscoarse/ageasentered
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE),
            .groups = "drop") %>%
  reshape_msd(include_type = FALSE) %>% 
  pivot_wider(
    names_from = indicator,
    names_glue = "{tolower(indicator)}") %>%
  arrange(desc(period)) #%>% View()

# VIZ ============================================================================
top_ou_list <- c("Burundi", "Eswatini", "Lesotho", "South Africa", "Uganda", "Zimbabwe")

#Children <15   
df_txnew %>% 
  mutate(operatingunit = case_when(operatingunit %in%
                                     c("Democratic Republic of the Congo") ~ "DRC",
                                   TRUE ~ operatingunit)) %>%
  filter(period %ni% c("FY22Q1", "FY22Q2", "FY22Q3", "FY22Q4"),
    operatingunit %in% top_ou_list#c("Kenya", "Malawi", "Mozambique", "Nigeria",
                             # "South Africa", "Tanzania", "Uganda", "Zambia",
                              #"Zimbabwe")
) %>% 
  ggplot(aes(x = period)) +
  geom_col(aes(y = tx_new), fill = "#1e87a5", width = 0.75,
           position = position_nudge(x = 0.1), alpha = 0.75) +
  geom_col(aes(y = tx_net_new), fill = "#83dbfb", width = 0.75, alpha = 0.75) +
  geom_text(aes(y = tx_net_new,
                label = comma(tx_net_new)),
            size = 11/.pt, 
            family = "Source Sans Pro", 
            color = grey90k,
            vjust = -0.5) +
  facet_wrap(~operatingunit,
             nrow = 3,
             scales = "free_y")+
  si_style_ygrid()+
  scale_x_discrete(
    breaks = every_nth(n = 2) #adds space btwn breaks 
    #labels = pd_brks
  ) +
  scale_y_continuous(expand = expansion(mult = c(0,0.15))#labels = label_number_si()
  )+
  #coord_cartesian(expand = T)+
  theme(plot.title = ggtext::element_markdown()) +
  labs(x = NULL, y = NULL, 
       title = glue("<span style = 'color:#1e87a5'>TX_NEW</span> 
                        & <span style = 'color:#83dbfb'>TX_NET_NEW</span> 
                        TRENDS BY OU| PEDS (<15 yo)"),
       caption = glue::glue("{meta$caption}")
  )

si_save(glue("Images/{meta$curr_pd}_TX_NEW_OU_trends_Peds.png"),
        scale = 1.25)

#Adolescent 15-19  
df_txnew %>% 
  mutate(operatingunit = case_when(operatingunit %in%
                                     c("Democratic Republic of the Congo") ~ "DRC",
                                   TRUE ~ operatingunit)) %>%
  filter(operatingunit %in% c("Kenya", "Malawi", "Mozambique", "Nigeria",
                              "South Africa", "Tanzania", "Uganda", "Zambia",
                              "Zimbabwe")) %>% 
  ggplot(aes(x = period)) +
  geom_col(aes(y = tx_new), fill = "#1e87a5", width = 0.75,
           position = position_nudge(x = 0.1), alpha = 0.75) +
  geom_col(aes(y = tx_net_new), fill = "#83dbfb", width = 0.75, alpha = 0.75) +
  geom_text(aes(y = tx_net_new,
                label = comma(tx_net_new)),
            size = 11/.pt, 
            family = "Source Sans Pro", 
            color = grey90k,
            vjust = -0.5) +
  facet_wrap(~operatingunit,
             nrow = 3,
             scales = "free_y")+
  si_style_ygrid()+
  scale_x_discrete(
    breaks = every_nth(n = 2) #adds space btwn breaks 
    #labels = pd_brks
  ) +
  scale_y_continuous(#labels = label_number_si()
  )+
  coord_cartesian(expand = T)+
  theme(plot.title = ggtext::element_markdown()) +
  labs(x = NULL, y = NULL, 
       title = glue("<span style = 'color:#1e87a5'>TX_NEW</span> 
                        & <span style = 'color:#83dbfb'>TX_NET_NEW</span> 
                        TRENDS BY OU| ADOLESCENTS (15-19 yo)"),
       caption = glue::glue("{meta$caption}")
  )

si_save(glue("Images/{meta$curr_pd}_TX_NEW_OU_trends_Adol.png"),
        scale = 1.25)

# SPINDOWN ============================================================================
