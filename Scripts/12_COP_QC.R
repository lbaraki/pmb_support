# PROJECT: COP/ROP
# PURPOSE: Munge and Analysis of OVC_SERV
# AUTHOR:  Lemlem Baraki | SI
# REF ID:  365ccbaa
# LICENSE: MIT
# DATE:   2024-11-18
# NOTES:   

# LOCALS & SETUP ============================================================================

  # Libraries
  library(tidyverse)
  library(glue)
  #oha
  library(gagglr) ##install.packages('gagglr', repos = c('https://usaid-oha-si.r-universe.dev', 'https://cloud.r-project.org'))
  #viz extensions
  library(scales, warn.conflicts = FALSE)
  library(systemfonts)
  library(tidytext)
  library(patchwork)
  library(ggtext)
    
  # SI specific paths/functions  
    load_secrets()
    path_msd <- si_path() %>% return_latest("PSNU_IM")
    
  # Grab metadata
   meta <- get_metadata(path_msd)
  
  # REF ID for plots
    ref_id <- "365ccbaa"
    
  # Functions  
    cop_ous <- glamr::pepfar_country_list %>% 
      filter(str_detect(operatingunit, "Region", negate = T)) %>% 
      pull(operatingunit)

# LOAD DATA ============================================================================  

  df <- read_psd(path_msd)

# MUNGE ============================================================================
  
  #OVC SERV metric:% OVC <18 enrolled in OVC Comprehensive program w/ status reported 
    #OVC_HIVSTAT pos + OVC_HIVSTAT neg + OVC_HIVSTAT not required/OVC_HIVSTAT_D
      #Numerator = # OVC enrolled with reported status 
        #standarddisagg: "Age/Sex/ReportedStatus", "Total Numerator", "Total Denominator"
        #otherdisagg: "Test Not Required"
      #Denominoator = # OVC reported under OVC_SERV
  
     #Option 1 - OVC_SERV Target Achievement (Targets/Results) for <18 
      #QC against Tableau workbook - https://tableau.usaid.gov/#/views/OVCDashboard/OVC_SERVAchievement?:iid=1
      
    df_ovc <- df %>% 
      filter(indicator %in% c("OVC_SERV_UNDER_18"),
             standardizeddisaggregate == "Total Numerator",
             fiscal_year == meta$curr_fy,
             funding_agency %in% c("USAID", "HHS/CDC")) %>%
      clean_agency() %>% 
      group_by(fiscal_year, country, psnu, psnuuid, funding_agency, indicator) %>% 
      summarise(across(c(targets, cumulative), ~ sum(.x, na.rm = TRUE)),
                .groups = "drop") %>% 
      calc_achievement() %>% View()
      mutate(type = "Total") %>% 
      select(country, psnu, psnuuid, funding_agency, indicator, achievement, type) %>% 
      filter(!is.na(achievement)) 
    
    
    
    #Option 2 - Known Status Proxy: assess all instances of "HIV Status Unknown" by targeting missing data/nondisclosure/reporting issues
      #Calculation: OVC_HIVSTAT_POS + OVC_HIVSTAT_NEG + OVC_HIVSTAT (Test Not Required)/OVC_SERV
        #Filter for <18, as this age group remains a priority  
      #QC against Tableau workbook - https://tableau.usaid.gov/#/views/OVCDashboard/KnownStatusProxy?:iid=1
        #Note: this would be from Q2
    prep_knownstatus <- df %>% 
        clean_agency() %>% 
        filter(fiscal_year == meta$curr_fy,
               funding_agency  %in% c("USAID", "CDC"),
               trendscoarse == "<18",
          (#Denominator
            (indicator == "OVC_SERV" & standardizeddisaggregate == "Age/Sex/ProgramStatus")|
            #Numerator - P + N + Test Not Required
            indicator == "OVC_HIVSTAT" & standardizeddisaggregate == "Age/Sex/ReportedStatus" & otherdisaggregate == "Test Not Required"|
            indicator %in% c("OVC_HIVSTAT_POS", "OVC_HIVSTAT_NEG"))
             ) %>% 
        clean_indicator() %>% 
        group_by(fiscal_year, country, psnu, psnuuid, funding_agency, indicator) %>% 
        summarise(across(c(cumulative), ~ sum(.x, na.rm = TRUE)),
                  .groups = "drop") %>%
        pivot_wider(names_from = indicator, 
                    values_from = cumulative,
                    names_glue = "{tolower(indicator)}") %>%
        mutate(achievement = (ovc_hivstat + ovc_hivstat_neg + ovc_hivstat_pos)/ovc_serv) %>% 
        mutate(type = "Total", indicator = "Known Status Proxy") %>% 
        select(country, psnu, psnuuid, funding_agency, indicator, achievement, type) %>% 
        filter(!is.na(achievement))
        
    
    #EID_COV <2 months 
    #note: numerator is of the first test, denominator is sum of STAT_POS and TST_POS_POSTANC1
    #QC against Tableau workbook - https://tableau.usaid.gov/#/views/DRAFT-PVTQuarterlyWorkbookFORFY23Q4RELEASE/EIDLinkagetoART?:iid=1
    prep_eid_cov <- df %>%
      clean_agency() %>% 
      filter(indicator %in% c("PMTCT_EID"),
             fiscal_year == meta$curr_fy,
             funding_agency %in% c("USAID", "CDC"),
             standardizeddisaggregate %in% c("Total Denominator", "Age/EID"),
             (#Rule: If "Age/EID", then "<2months"
               (standardizeddisaggregate == "Age/EID" & ageasentered == "<=02 Months")|
                 standardizeddisaggregate == "Total Denominator")
      ) %>% 
      gophr::clean_indicator() %>% 
      group_by(fiscal_year, country, psnu, psnuuid, funding_agency, indicator) %>%
      summarise(across(c(cumulative), ~ sum(.x, na.rm = TRUE)),
                .groups = "drop") %>% #View()
      pivot_wider(names_from = indicator, # Pivot wide to perform calculations
                  values_from = cumulative,
                  names_glue = "{tolower(indicator)}") %>% 
      mutate(achievement = pmtct_eid/pmtct_eid_d) %>%
      mutate(type = "Total", indicator = "PMTCT_EID <2 months") %>% 
      select(country, psnu, psnuuid, funding_agency, indicator, achievement, type) %>% 
      filter(!is.na(achievement)) 
        
      

# SPINDOWN ============================================================================

