## Sensitivity analyses: Assuming unequal contribution of sources to distribution system
  # based on source average
## Drinking water and PTB sybling analysis
## Author: A. Sherris


# SENSITIVITY ANALYSIS: Assume more frequently sampled sources contribute more to supply

# function: generate mean tap water concentration for given chemical and maternal pregnancy interval
# uses expanded pregnancy interval if there are no data within the pregnancy 
# designed for use with water_data object that is ALREADY subsetted to the chemical of interest
# (the assign_exp_by_county functionsubsets water_data to county and chemical)


assign_gest_exposure_simple <- function(water_data, mat_pwsid, preg_int, preg_int_long) {
  
  # find the average contaminant concentration during pregnancy
  
  # find all data from maternal pwsid generated within pregnancy interval
  pws_data <- water_data %>% 
    filter(pwsid == mat_pwsid,
           date %within% preg_int)

  # if data are available during pregnancy:  
  if(nrow(pws_data) > 0) {

    exp_start <- as.Date(int_start(preg_int))
    exp_end <- as.Date(int_end(preg_int))
    
      # AND if data available from distribution system sources are available, use only these data

    if("DT" %in% pws_data$status | "DR" %in% pws_data$status) {
      pws_data <- pws_data %>% 
        # filter to distribution system sources 
        filter(status %in% c("DT", "DR"))
    }
        
    # find the time-weighted average of sources during pregnancy
    pws_avg <- pws_data %>% 
      # remove sources that have a receiving source for which there is also data
      filter(!source %in% pws_data$source[pws_data$rec_source %in% pws_data$source]) %>% 
      # some sources have multiple findings for the same day - these are averaged
      group_by(source, date) %>% 
      summarise(source_mean_date = mean(finding)) %>% 
      ungroup() %>% 
      # calculate the number of useful data points (one for each source per day)
      mutate(samples = n()) 
    
    conc <- mean(pws_avg$source_mean_date)
    sample_n <- first(pws_avg$samples) #original number of samples available
    source_n <- nrow(pws_avg) # number of sources that contribute to the estimate
    uncertainty_sd <- sd(pws_avg$source_mean_date, na.rm = T)
    uncertainty_se <- uncertainty_sd / sqrt(source_n)
    interval <- "pregnancy"
    
    return(paste(conc, sample_n, source_n, uncertainty_sd, uncertainty_se, interval))
    
  } else {
  
    # find the average contaminant concentration duuring EXTENDED pregnancy interval
    pws_data <- water_data %>% 
      filter(pwsid == mat_pwsid,
             date %within% preg_int_long) 
    
    exp_start <- as.Date(int_start(preg_int_long))
    exp_end <- as.Date(int_end(preg_int_long))
    
    # AND if data available from distribution system sources are available, use only these data
    
    if("DT" %in% pws_data$status | "DR" %in% pws_data$status) {
      pws_data <- pws_data %>% 
        # filter to distribution system sources 
        filter(status %in% c("DT", "DR"))
    }
    
    # find the time-weighted average of sources during pregnancy
    pws_avg <- pws_data %>% 
      # remove sources that have a receiving source for which there is also data
      filter(!source %in% pws_data$source[pws_data$rec_source %in% pws_data$source]) %>% 
      # some sources have multiple findings for the same day - these are averaged
      group_by(source, date) %>% 
      summarise(source_mean_date = mean(finding)) %>% 
      ungroup() %>% 
      # calculate the number of useful data points (one for each source per day)
      mutate(samples = n())
    
    conc <- mean(pws_avg$source_mean_date)
    sample_n <- first(pws_avg$samples) #original number of samples available
    source_n <- nrow(pws_avg) # number of sources that contribute to the estimate
    uncertainty_sd <- sd(pws_avg$source_mean_date, na.rm = T)
    uncertainty_se <- uncertainty_sd / sqrt(source_n)
    interval <- "long"
    
    return(paste(conc, sample_n, source_n, uncertainty_sd, uncertainty_se, interval))
  }
}
 