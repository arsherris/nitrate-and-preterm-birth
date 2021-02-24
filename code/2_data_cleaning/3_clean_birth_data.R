## CLEAN BIRTHS DATA
## Nitrate in drinking water and spontaneous preterm birth
## Author: A. Sherris

# function: clean_births -----------------------------------------------------------------------
  # input: raw births data from state
  # output: cleaned births data

clean_births <- function(data_births_raw) {
  
data_births_raw %>% 
    
  # rename variables
  rename(mat_educ            = medu_3cat,
         mat_age             = mothage,
         mat_age_5cat        = mothage_5cat,
         mat_race            = race,
         precare             = precare_5,
         payer               = delpay,
         tract_poverty       = Tract_PovertyRatePer1000,
         final_gest_wk       = Final_gestwk,
         final_gest_wk_valid = Final_gestwk_valid,
         prem_5cat           = Prem_5Cat,
         prem_2cat           = Prem_2Cat,
         mat_id              = brthidHST,
         birth_id            = brthid) %>%
    select(-birthord, birtype, PLURA) %>% 
    
    # clean variables
    mutate(bdate         = as.Date(bdate, format = "%m/%d/%Y"),
           doc           = as.Date(doc, format = "%m/%d/%Y"),
           mat_age_5cat  = factor(mat_age_5cat,
                                  levels = 1:5,
                                  labels = c("<20", "20-24", "25-29", "30-34", ">=35")),
           mat_race      = factor(mat_race, 
                                  levels = c(2,1,4,3,5),
                                  labels = c("Hispanic", "White", "Asian", "Black", "Other")),
           mat_educ      = factor(mat_educ,
                                  levels = 1:3,
                                  labels = c("<12 years", "12 years", ">12 years")),
           precare       = factor(precare,
                                  levels = c(0,1),
                                  labels = c("Yes", "No")),
           payer         = factor(payer, 
                                  levels = 1:4,
                                  labels = c("Medi-Cal", "Private", "Uninsured", "Other")),
           year          = year(bdate),
           month         = month(bdate),
           prem_20_to_27 = case_when(prem_5cat %in% c(1,2) ~ 1,
                                     prem_5cat == 5 ~ 0,
                                     TRUE ~ NA_real_),
           prem_28_to_31 = case_when(prem_5cat == 3 ~ 1,
                                     prem_5cat == 5 ~ 0,
                                     TRUE ~ NA_real_),
           prem_20_to_31 = case_when(prem_5cat %in% c(1:3) ~ 1,
                                     prem_5cat == 5 ~ 0,
                                     TRUE ~ NA_real_),
           prem_32_to_36 = case_when(prem_5cat == 4 ~ 1,
                                     prem_5cat == 5 ~ 0,
                                     TRUE ~ NA_real_)) %>%
    
    # reorder variables
    select(birth_id, mat_id, bdate, doc, starts_with("mat_"), precare, payer, sex, tract_poverty, everything()) %>% 
    return()
}

# function: spatial_births ------------------------------------------------------------------

# transform data into spatial points data frame with projected CRS and find county

spatial_births <- function(data_births_clean) {
  
  data_births_clean %>% 
    
    # remove births with missing coordinates
    filter(!is.na(long)) %>% 
    
    # transform to spatial projected
    st_as_sf(coords = c("long", "lat"), crs = crs_geo) %>% 
    st_transform(crs_projected) %>% 
    
    # determine counties in which maternal residences are located 
    st_join(ca_counties, 
            join = st_intersects) %>% 
    
    # a few births are on border of two counties; select first listed
    group_by(birth_id) %>%
    mutate(count = 1:n()) %>%
    filter(count == 1) %>%
    select(-count) %>%
    ungroup() %>% 
    return()
}


# end