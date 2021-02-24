## RUN FUNCTIONS TO CLEAN DATA
## Drinking water and PTB sybling analysis
## Author: A. Sherris

# load functions
source('code/3_data_cleaning/2_clean_birth_data.R')
source('code/5_analysis/3_sensitivity/4_comorbidities/2_comorb_inclusion_criteria.R')

# run functions

    # clean
    births <- clean_births(births_raw)
    
    births_comorb <- comorb_inclusion_study_pop(births)
    
    rm(births, births_raw)
    
    # generate spatial df and find county
    births_comorb_sp <- spatial_births(births_comorb)
    
    births_comorb_sp <- births_comorb_sp %>% 
      filter(!is.na(county))
   
  # save clean dataset
    save(births_comorb, file = "data/processed/clean_input/births_comorb.RData")
    save(births_comorb_sp, file = "data/processed/clean_input/births_comorb_sp.RData")
