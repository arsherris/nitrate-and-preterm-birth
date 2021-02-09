## DEFINE GESTATIONAL EXPOSURE BASED ON MATERNAL PWSIDS
## Nitrate in drinking water and spontaneous preterm birth
## Author: A. Sherris

# function: run county-by-county exposure assessment with progress bar

assign_exp_with_progress <- function(county){
  
  pb$tick()$print()
  
  assign_exp_by_county(exposure_assign_fxn = assign_gest_exposure, 
                       births_pws_data = births_interval, 
                       chemical = study_exposure, 
                       county_name = county)
}


# function: divide data into counties and assign gestational drinking water concentrations

assign_exp_by_county <- function(exposure_assign_fxn, births_pws_data, chemical, county_name) {
  

  # restrict all data to county of interest
  births_pws_county <- filter(births_pws_data, county == county_name)
  wq_data_county <- filter(wq_data, 
                           county == county_name,
                           chem == chemical)
  
  # assign gestational drinking water concentrations
  births_exp <- births_pws_county %>%
    rowwise() %>%
    mutate(conc = exposure_assign_fxn(water_data = wq_data_county,
                                      mat_pwsid = pwsid,
                                      preg_int = preg_int,
                                      preg_int_long = preg_int_long)) %>% 
    ungroup() %>%
    select(birth_id, pwsid, conc) %>% # 2020-04-09 - removed area, population from list of variables (if these are needed, get from pws_info)
    mutate(chem = chemical) %>% 
    return()
  
}

# function: generate mean tap water concentration for given chemical and maternal pregnancy interval
# uses expanded pregnancy interval if there are no data within the pregnancy 
# designed for use with water_data object that is ALREADY subsetted to the chemical of interest
# (the assign_exp_by_county functionsubsets water_data to county and chemical)

assign_gest_exposure <- function(water_data, mat_pwsid, preg_int, preg_int_long) {
  
  # find the average contaminant concentration during pregnancy
  
  # find all data from maternal pwsid generated within pregnancy interval
  pws_data <- water_data %>% 
    filter(pwsid == mat_pwsid,
           date %within% preg_int)
  
  if(nrow(pws_data) > 0) {
    
    # average sources within pregnancy
    pws_avg <- pws_data %>% 
      ### SHOULD PRIORITIZE DISTRIBUTION SAMPLES IF AVAILABLE!!
      # remove sources that have a receiving source for which there is also data
      filter(!source %in% pws_data$source[pws_data$rec_source %in% pws_data$source]) %>% 
      # calculate the number of useful data points (one for each source per day)
      mutate(samples = n()) %>% 
      # average each source so that sources are counted once toward the final estimate
      group_by(source) %>% 
      summarise(source_mean = mean(finding, na.rm = T),
                samples = first(samples)) %>% 
      ungroup()
    
    conc <- mean(pws_avg$source_mean, na.rm = T)
    sample_n <- first(pws_avg$samples)
    source_n <- nrow(pws_avg) # number of sources that contribute to the estimate
    interval <- "pregnancy"
    
    return(paste(conc, sample_n, source_n, interval))
    
  } else {
    
    
    # find the average contaminant concentration duuring EXTENDED pregnancy interval
    pws_data <- water_data %>% 
      filter(pwsid == mat_pwsid,
             date %within% preg_int_long) 
    
    
    # if data are available during extended pregnancy interval:
    if(nrow(pws_data) > 0) {
      
    # remove sources that have a receiving source for which there is also data
    # average sources within pregnancy
    pws_avg <- pws_data %>% 
      # remove sources that have a receiving source for which there is also data
      filter(!source %in% pws_data$source[pws_data$rec_source %in% pws_data$source]) %>% 
      # calculate the number of useful data points (one for each source/day)
      mutate(samples = n()) %>% 
      # average each source so that sources are counted once toward the final estimate
      group_by(source) %>% 
      summarise(source_mean = mean(finding, na.rm = T),
                samples = first(samples)) %>% 
      ungroup()
    
    conc <- mean(pws_avg$source_mean, na.rm = T)
    sample_n <- first(pws_avg$samples)
    source_n <- nrow(pws_avg) # number of sources that contribute to the estimate
    interval <- "long"
    
    return(paste(conc, sample_n, source_n, interval))
    
    } else 
      
    conc <- NA
    sample_n <- NA
    source_n <- NA
    interval <- "none"
    
    return(paste(conc, sample_n, source_n, interval))
  }
}
 