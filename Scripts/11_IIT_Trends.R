# PROJECT: GH Portfolio Review: IIT Volume 
# PURPOSE: Munge and Analysis of
# AUTHOR:  L. Baraki & N. Maina | SI
# REF ID:  139dd88c
# LICENSE: MIT
# DATE:   2024-06-24
# NOTES:   

# LOCALS & SETUP ============================================================================

# Libraries
library(glamr)
library(tidyverse)
library(glitr)
library(gophr)
library(systemfonts)
library(scales)
library(tidytext)
library(patchwork)
library(ggtext)
library(glue)
library(readxl)
library(googlesheets4)

# SI specific paths/functions  
load_secrets()
merdata <- file.path(glamr::si_path("path_msd"))
file_path <- return_latest(folderpath = merdata,
                           pattern = "OU_IM_FY22")

# Grab metadata
metadata <- get_metadata(file_path)

# REF ID for plots
ref_id <- "139dd88c"

# Functions  
#IIT volume
#standarddisaggregate:  Age/Sex/ARTNoContactReason/HIVStatus
#otherdisaggregate: No Contact Outcome - Interruption in Treatment (<3, 3-5, 6+ Months)

#create_iit_vol <- function(df, ...) {
# df <- df %>%
#  filter(
#   indicator %in% c("TX_ML"),
#  standardizeddisaggregate %in% c("Age/Sex/ARTNoContactReason/HIVStatus"),
# str_detect(otherdisaggregate, "No Contact Outcome - Interruption")
#) %>%
#gophr::clean_indicator() %>%
#group_by(indicator, fiscal_year, ...) %>%
#summarise(across(starts_with("qtr"), sum, na.rm = TRUE), 
#        .groups = "drop") %>%
#reshape_msd(include_type = FALSE) %>%
#pivot_wider(
# names_from = otherdisaggregate,
#names_glue = "{tolower(otherdisaggregate)}"
#) %>%
#rename(`6+ months treatment` = `no contact outcome - interruption in treatment 6+ months treatment`) %>% 
#group_by(...) %>% 
#mutate(
# total = sum(No Contact Outcome - Interruption in Treatment <3 Months Treatment),
#)) %>% 
#ungroup() %>% 
#return(df)
#}    

# LOAD DATA ============================================================================  

df <- read_psd(file_path) %>% 
  clean_agency()

# MUNGE ============================================================================

#Limit 
#df_iit <- df %>% 
#filter(funding_agency == "USAID",
#      indicator %in% c("TX_ML", "TX_CURR", "TX_NEW"),
#     operatingunit != "Ukraine", 
#    fiscal_year >= 2022,
#   standardizeddisaggregate %in% c("Age/Sex/HIVStatus",
#                                  "Age/Sex/CD4/HIVStatus",
#                                 "Age/Sex/ARTNoContactReason/HIVStatus"))


#IIT volume
df_vol <- df %>% 
  filter(funding_agency == "USAID",
         indicator %in% c("TX_ML"),
         operatingunit != "Ukraine", 
         fiscal_year >= 2022,
         standardizeddisaggregate %in% c("Age/Sex/ARTNoContactReason/HIVStatus"),
         str_detect(otherdisaggregate, "Interruption")
  ) %>% 
  mutate(otherdisaggregate = str_remove(otherdisaggregate,
                                        "No Contact Outcome - Interruption in Treatment "))%>% 
  group_by(indicator,fiscal_year, operatingunit, otherdisaggregate) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), 
            .groups = "drop") %>% 
  reshape_msd(include_type = F)

df_vol_agency <- df_vol%>% 
  group_by(period) %>% 
  summarise(across(c("value"), sum,na.rm = TRUE), .groups = "drop")%>% 
  rename(USAID_share = value) %>% 
  arrange(desc(period)) 

df_vol_ou <- df_vol %>% 
  left_join(df_vol_agency, by = "period") %>% #total is just IIT across periods
  pivot_wider(
    names_from = otherdisaggregate,
    names_glue = "{tolower(otherdisaggregate)}") %>% 
  rename(`6+ months treatment` = `no contact outcome - interruption in treatment 6+ months treatment`) %>% 
  rowwise() %>% 
  mutate(iit = sum(`3-5 months treatment` + `<3 months treatment` + `6+ months treatment`)) %>% 
  select(-c(`3-5 months treatment`,`<3 months treatment`,`6+ months treatment`)) %>% 
  arrange(desc(period)) 


# VIZ ============================================================================
#Identify the IIT share for the largest countries 
df_vol_ou %>% 
  filter(#operatingunit %in% c("Nigeria", "Mozambique", "South Africa")
    period == metadata$curr_pd) %>% 
  arrange(desc(iit)) %>% 
  slice_max(n = 5, order_by = iit) %>%
  mutate(share = percent(iit/USAID_share,.1))


#Trends over time - volume 
#x-axis: quarters
#y-axis: IIT Volume 
df_vol_ou %>%
  filter(period >= "FY23Q1") %>% 
  mutate(#fct_relevel(operatingunit, c("South Africa", "Mozambique", "Nigeria"))) %>% 
    operatingunit = factor(operatingunit, levels = c("South Africa", "Mozambique", "Nigeria"))) %>% 
  ggplot(aes(x = period)) + 
  geom_col(aes(y = iit), 
           fill = trolley_grey_light,
           alpha = .5) + 
  geom_col(data = . %>% filter(operatingunit %in% 
                                 c("Mozambique","Nigeria", "South Africa")),
           aes(y = iit, fill = operatingunit)
  ) + 
  #geom_text(aes(y = USAID_share,
  #             label = comma(USAID_share)), #USAID total
  #            size = 11/.pt,
  #       family = "Source Sans Pro",
  #          vjust = -.5,
  #         color = grey90k
  #        ) + 
  geom_text(data = . %>% filter(period == "FY24Q2",
                                operatingunit %in% c("Mozambique","Nigeria", "South Africa")),
            aes(y = iit,
                label = percent(iit/USAID_share)), 
            size = 11/.pt,
            family = "Source Sans Pro",
            vjust = 1.75,
            color = "white"
  ) + 
  scale_fill_manual(values = c("Nigeria" = glitr::burnt_sienna, 
                               "South Africa" = glitr::denim,
                               "Mozambique" = glitr::genoa),
                    name = "OU:"#rename the legend
  ) + 
  si_style_ygrid() + 
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()))+#scales::number
  theme(plot.title = ggtext::element_markdown(),
        plot.subtitle = ggtext::element_markdown(),
        #legend.position = "none"
  )+
  labs(x = NULL, y = NULL,
       title = "USAID IIT Trends from FY22 - FY24",
       caption = glue::glue("{metadata$caption}
                                Note: Nigeria data missing for FY23Q4")
  )


si_save("Graphics/IIT_Volume_FY24Q2.png")




# SPINDOWN ============================================================================

#proxy IIT: TX_ML/TX_CURR_lag1 + TX_NEW
#create_iit_df <- function(df, ...) {
# df <- df %>%
#  filter(
#   indicator %in% c("TX_ML", "TX_CURR", "TX_NEW"),
#  standardizeddisaggregate %in% c(
#   "Age/Sex/HIVStatus",
#  "Age/Sex/CD4/HIVStatus",
# "Age/Sex/ARTNoContactReason/HIVStatus"
#)
#) %>%
#gophr::clean_indicator() %>%
#group_by(indicator, fiscal_year, ...) %>%
#summarise(across(starts_with("qtr"), sum, na.rm = TRUE), 
#         .groups = "drop") %>%
#reshape_msd(include_type = FALSE) %>%
#pivot_wider(
# names_from = indicator,
#names_glue = "{tolower(indicator)}"
#) %>%
#group_by(...) %>% 
#mutate(
# tx_curr_lag1 = lag(tx_curr, n = 1),
#iit = tx_ml/(tx_curr_lag1 + tx_new)
#) %>% 
#ungroup() %>% 
#return(df)
#}    

#IIT percentage
#iit_percent <- create_iit_df(df) %>%
# arrange(desc(period)) 

#iit_percent_OU <- create_iit_df(df, operatingunit) %>% 
# arrange(desc(period))

#Trends over time - percentage
#x-axis: 6 quarters (FY22Q3 - FY24Q2)
#y-axis: IIT percentage

#num_pds <- length(unique(iit_percent$period))

#iit_percent %>% 
# filter(period >="FY22Q3") %>% 
#ggplot(aes(x = period, y = iit, group = "a")) +
#geom_area(fill = grey20k, alpha = 0.75) +
#geom_line(#aes(y = iit), 
# color = denim,
#linewidth = 0.8) +
#geom_point(#aes(y = iit), 
# color = denim) +
#geom_text(aes(y = iit, label = percent(iit,.1)),
#         color = denim,
#        vjust = -2, 
#       size = 10/.pt,
#      family = "Source Sans Pro SemiBold") +
#si_style_ygrid() + 
#expand_limits(y = c(0.0,.08)) +
#scale_y_continuous(labels = scales::percent) +
#theme(plot.title = ggtext::element_markdown(),
#     plot.subtitle = ggtext::element_markdown())+
#labs(x = NULL, y = NULL,
#   title = "IIT Trends from FY22 - FY24",
#    caption = glue::glue("{metadata$caption}")
#)

