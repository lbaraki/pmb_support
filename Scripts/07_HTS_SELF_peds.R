# PROJECT: PMB Requests
# PURPOSE: Munge and Analysis of HTS_SELF
# AUTHOR:  Lemlem Baraki | SI
# REF ID:  0ea200c0
# LICENSE: MIT
# DATE:   2024-04-09
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
file_path <- si_path() %>% return_latest("OU_IM_FY22-24") 

# Grab metadata
meta <- get_metadata(file_path)

# REF ID for plots
ref_id <- "0ea200c0"

# Functions  
#fifteen_ous <- c("Burundi","Democratic Republic of the Congo","Eswatini",
 #                "Haiti", "Kenya","Lesotho","Malawi","Mozambique",
  #               "Nigeria","South Africa","South Sudan","Tanzania",
   #              "Uganda","Zambia","Zimbabwe") 

# LOAD DATA ============================================================================  

df_msd <- read_psd(file_path) 
#filter(indicator %in% c("HTS_SELF")) %>% 
#select(standardizeddisaggregate, otherdisaggregate, otherdisaggregate_sub) %>% View()

#HTS_SELF
#standarddisagg is "HIVSelfTestUser"
#otherdisagg is "Unassisted - Caregiver for Child"
#Options include: Self, Sex Partner, Caregiver for Child, Other 

df_msd_limit <- df_msd%>% 
  filter(indicator %in% c("HTS_SELF"),
         standardizeddisaggregate %in% c("HIVSelfTestUser"),
         otherdisaggregate %in% c("Unassisted - Caregiver for Child"),
         fiscal_year %in% c(2024),
         #trendscoarse == "<15"
         #operatingunit %in% fifteen_ous
  )  


# MUNGE ============================================================================

#Index Testing  
df_final <- df_msd_limit %>% 
  clean_agency() %>% 
  #filter(indicator %in% c("HTS_SELF")) %>% 
  group_by(fiscal_year, operatingunit, funding_agency, otherdisaggregate, indicator) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% #glimpse()
  reshape_msd(include_type = FALSE) %>% 
  pivot_wider(names_from = "funding_agency",
              names_glue = "{toupper(funding_agency)}") %>% 
  mutate_all(replace_na, replace = 0) %>% 
  mutate(Total = CDC + USAID + DOD + STATE) %>% 
  arrange(desc(Total)) #%>% View()

# VIZ ============================================================================

#Horizontal Bar Chart w/ Totals 
#HTS_SELF: # of individual HIV self-test kits distributed
#Unassisted - Caregiver for Child: 
#secondary distribution of HIV self-test kits occur by parents or caregivers of children >=2 with an unknown HIV status

df_final %>% 
  mutate(operatingunit = case_when(operatingunit %in%
                                     c("Democratic Republic of the Congo") ~ "DRC",
                                   TRUE ~ operatingunit)) %>%
  mutate(cntry_order = fct_reorder(operatingunit, Total)) %>% 
  ggplot(aes(x = cntry_order, y = Total, 
  )) +
  geom_col(fill = glitr::denim, alpha = 0.95)+
  coord_flip()+
  geom_text(aes(y = Total,
                label = comma(Total)),
            size = 12/.pt,
            family = "Source Sans Pro",
            color = grey90k,
            hjust = -0.25)+
  scale_y_continuous()+
  si_style_xgrid()+
  labs(title = "HTS_SELF - Unassisted by Caregiver for Child | FY24Q1",  
       #subtitle = "***",
       y = "Number of Self Testing Kits Distributed", x = NULL,
       caption = glue::glue("{meta$caption}")) 

si_save("Images/HTS_SELF_Caregiver_disagg_barchart.png")

#V2 - GT tables of agency breakdown
df_final %>% 
  select(-c(period, indicator, otherdisaggregate)) %>% 
  mutate(operatingunit = case_when(operatingunit %in%
                                     c("Democratic Republic of the Congo") ~ "DRC",
                                   TRUE ~ operatingunit)) %>%
  gt(
    #groupname_col = c("otherdisaggregate")
  ) %>% 
  fmt_number(columns = 2:6,
             decimals = 0) %>% 
  gt_hulk_col_numeric(6, trim = TRUE, ) %>% 
  cols_label(operatingunit = "OU") %>% 
  #grand_summary_rows(columns = 2:6,
  #                  locations = cells_grand_summary()) %>% 
  tab_source_note(
    source_note = glue("{meta$caption} ")) %>% 
  tab_options(
    source_notes.font.size = px(10)) %>% 
  tab_header(
    title = glue("HTS_SELF: Unassisted - Caregiver for Child"),
  ) %>% 
  gt_theme_nytimes() %>% 
  gtsave("Images/HTS_SELF_Caregiver_disagg_table.png")

# SPINDOWN ============================================================================
