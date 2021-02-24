## CLEAN DATA FROM PUBLIC WATER SYSTEMS
## Nitrate in drinking water and spontaneous preterm birth
## Author: A. Sherris


# function: clean water quality data
  # input: raw water quality from State Water Resources Control Board
  # output: cleaned water quality
  
clean_wq_data <- function(data_wq_raw) {
  
  # load and clean flow paths
  
    flow_paths <- flow_paths_raw %>% 
      rename(pwsid =  NUMBER0,
             sup_id = `Supply ID`,
             rec_id = `Receiving ID`
             ) %>% 
      mutate(source = paste(substr(pwsid, 3,9), sup_id, sep = "-"),
             rec_source = paste(substr(pwsid, 3,9), rec_id, sep = "-")) %>% 
      select(source, rec_source)
    
  # water quality data
  
    # find the most common MDL for each contaminant
    # these will be used to replace "0" values for nondetects
    
      mdls <- data_wq_raw %>% 
        filter(XMOD == "<",
               FINDING != 0) %>%
        group_by(CHEMICAL, FINDING) %>% 
        summarise(count = n()) %>% 
        top_n(1, count) %>% 
        select(CHEMICAL, common_mdl = FINDING) %>% 
        ungroup()
      
    # clean water quality data
    # replace "0" value NDs with most common MDL
    
    wq_data <- data_wq_raw %>% 
      
      # rename chemical of interest
      mutate(CHEMICAL == "Nitrate") %>% 
      
      # join with common MDL data
      left_join(mdls, by = "CHEMICAL") %>% 
        
      # join with source and pws info
      left_join(source_status, by = c("PRIM_STA_C" = "PRI_STA_C")) %>% 
      left_join(select(pws_info, pwsid = `Water System No`,
                       county = `Principal County Served`)) %>% 
      select(pwsid, 
             county,
             source  = PRIM_STA_C, 
             chem    = CHEMICAL, 
             date    = SAMP_DATE, 
             xmod    = XMOD, 
             mcl     = MCL,
             finding = FINDING, 
             status  = STATUS,
             common_mdl) %>% 
      
      # remove if nondetect and reported finding (RL) is greater than half the MCL
      mutate(bad_mcl = ifelse(xmod == "<" & finding > (mcl/2) & mcl != 0, 1, 0)) %>%
      filter(bad_mcl == 0 | is.na(bad_mcl),
             
             # restrict to mapped pwsids and study time period 
             pwsid %in% pws_sp$pwsid, 
             date >= as.Date("1999-01-01"),
             date <= as.Date("2013-01-01"),
             
             # remove invalid qualifiers
             xmod == "<" | is.na(xmod), 
            
             # remove NA findings
             !is.na(finding),
             
             # remove agricultural and monitoring well data
             !status %in% c("AG", "MW"),
             
             # nitrate specific: remove outliers over 2000 mg/L
             finding <= 2000) %>%  
      
      # clean minimum detection limits
      mutate(finding = case_when(
        
        # if ND and MDL given, use 1/2 given MDL 
        xmod == "<" & finding != 0 ~ finding / 2,
        
        # if ND and no MDL given, use 1/2 the most common MDL for that contaminant
        finding == 0 ~ common_mdl / 2,  ### this applies to all 0 findings, even if xmod is not "<"
        
        TRUE ~ finding),
        order = 1:n()) %>% 
      
      # retain one sample per date
      group_by(source, date) %>%
      top_n(1, order) %>% 
      ungroup() %>% 
      select(-common_mdl, -bad_mcl, -order) %>%
      left_join(flow_paths, by = "source") %>% 
      unique
    

    return(wq_data)
  }
  
   
# end