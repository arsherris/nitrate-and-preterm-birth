## RUN FUNCTIONS TO ASSIGN PWSIDS
## Nitrate in drinking water and spontaneous preterm birth
## Author: A. Sherris

# load functions
  source("code/3_exposure_assessment/1_assign_maternal_PWS.R")
  
# load clean PWS data
  load("data/processed/clean_input/pws_clean.RData")

# load study population
  load("data/processed/clean_input/births_study_pop_sp.RData")
  
### assign maternal public water system to births based on WBT boundaries

  # generate list of counties in birth data;
  # these will be used to run county-by-county exposure assessment functions
  county_list <- unique(births_study_pop_sp$county)
  
  # assign maternal PWS (county by county)
  births_study_pop_pws <- lapply(county_list,
                     FUN = function(x) 
                       assign_pws_by_county(births_study_pop_sp, pws_sp, x)) %>% bind_rows()
  
  # save maternal PWS output
  births_study_pop_in_pws_boundary <- births_study_pop_pws %>% 
    filter(!is.na(pwsid)) %>% 
    select(birth_id, pwsid)
  

### for births that are not within WBT boundary, assign PWS based on 0.5 km buffer
  
  # create PWS sp object with 0.5 km buffer
  pws_buffer_sp <- pws_sp %>%
    # makes 0.5 km buffer
    st_buffer(dist = 500)   # distance in meters 
  
  # create sp object for births not assigned PWS
  births_study_pop_buffer_sp <- births_study_pop_sp %>% 
    filter(!birth_id %in% births_study_pop_in_pws_boundary$birth_id)
  
  # generate list of counties in birth data;
  # these will be used to run county-by-county exposure assessment functions
  county_list <- unique(births_study_pop_buffer_sp$county)
  
  # assign maternal PWS (county by county)
  births_study_pop_buffer_pws <- lapply(county_list,
                       FUN = function(x) 
                         assign_pws_by_county(births_study_pop_buffer_sp, 
                                              pws_buffer_sp, x)) %>% bind_rows()
  
  # save maternal PWS output
  births_study_pop_in_pws_buffer <- births_study_pop_buffer_pws %>% 
    filter(!is.na(pwsid)) %>% 
    select(birth_id, pwsid)
  

### merge PWS assignments with original birth data

  births_study_pop_pws_final <- births_study_pop_sp %>% 
    st_drop_geometry() %>% 
    left_join(births_study_pop_in_pws_boundary) %>% 
    left_join(rename(births_study_pop_in_pws_buffer, pwsid_buff = pwsid)) %>% 
    mutate(pws_type = case_when(
      !is.na(pwsid) ~ "wbt",
      !is.na(pwsid_buff) ~ "buffer",
      TRUE ~ "unassigned"),
      pwsid = as.character(pwsid),
      pwsid_buff = as.character(pwsid_buff),
      pwsid = case_when(
        !is.na(pwsid) ~ pwsid,
        !is.na(pwsid_buff) ~ pwsid_buff,
        TRUE ~ NA_character_)) %>% 
    select(-pwsid_buff)
  
# save data
  
  save(births_study_pop_in_pws_boundary, births_study_pop_in_pws_buffer,
       file = "data/processed/2_maternal_pwsid/birth_remainder_pws_intermediate.RData")
  
  save(births_study_pop_pws_final,
       file = "data/processed/2_maternal_pwsid/births_study_pop_pws_final.RData")
  
  rm(county_list, pws_buffer_sp, pws_sp,
     births_study_pop_pws, births_study_pop_sp,
     births_study_pop_buffer_sp, births_study_pop_in_pws_boundary, 
     births_study_pop_buffer_pws, births_study_pop_in_pws_buffer, 
     assign_pws, assign_pws_by_county)
  