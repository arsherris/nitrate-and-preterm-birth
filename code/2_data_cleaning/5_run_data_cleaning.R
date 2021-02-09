## RUN FUNCTIONS TO CLEAN DATA
## Nitrate in drinking water and spontaneous preterm birth
## Author: A. Sherris

# load necessary functions
source('code/2_data_cleaning/1_clean_pws_data.R')
source('code/2_data_cleaning/2_clean_wq_data.R')
source('code/2_data_cleaning/3_clean_birth_data.R')
source('code/2_data_cleaning/4_inclusion_criteria.R')

# run functions

  # water quality data
  
    # public water systems
    pws_sp <- clean_pws(pws_sp_raw)
    
    # water quality data
    wq_data <- clean_wq_data(wq_data_raw)  
    
    # remove raw data
    rm(mcls, flow_paths_raw, pws_info, pws_sp_raw, source_info, wq_data_raw)
    
  # birth data
  
    # clean
    births <- clean_births(births_raw)
    
    # generate spatial df and find county of each birth
    births_sp <- spatial_births(births)
    
    # join county to clean dataset
    births <- births %>% 
      left_join(select(births_sp, birth_id, county))
    
    # apply exclusion criteria to births data
    
    births_study_pop <- inclusion_study_pop(births)
    
    births_study_pop_sp <- births_study_pop %>% 
      # transform to spatial projected
      st_as_sf(coords = c("long", "lat"), crs = crs_geo) %>% 
      st_transform(crs_projected)

  # save clean datasets
    save(pws_sp, file = "data/processed/clean_input/pws_clean.RData")
    save(wq_data, file = "data/processed/clean_input/nitrate_wq_data.RData")
    save(births_study_pop, file = "data/processed/clean_input/births_study_pop.RData")
    save(births_study_pop_sp, file = "data/processed/clean_input/births_study_pop_sp.RData")

  # remove raw and unneeded data
  rm(births_raw, births_sp, births, births_study_pop, ca_counties, ca_counties_geo, 
     ca_state_geo, ca_state_proj, crs_geo, crs_projected, sjv_counties)  
  rm(clean_births, spatial_births, clean_pws, clean_wq_data, inclusion_study_pop)
