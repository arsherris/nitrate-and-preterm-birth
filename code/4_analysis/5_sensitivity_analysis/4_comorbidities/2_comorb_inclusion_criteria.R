## INCLUSION CRITERIA
## Drinking water and PTB sybling analysis
## Author: A. Sherris

# function: apply exclusion criteria to births data

comorb_inclusion_study_pop <-  function(data_births_cleaned) {
  
  data_births_cleaned %>%
    filter(
      
      # singletons
      birtype == 1,
      
      # non-na longitude
      !is.na(long),
      
      # valid gestational week 20-41 weeks
      final_gest_wk_valid == 1,
      
      # valid birth weight 500-5000 g
      bwt_range == 1,
      
      # geocode to state or zip code
      MATCHED != "City",
      
      # better link between OSHPD and birth certificate
      link == 1,
      
      #  bettter match with VSB, PDDI, and PDDM for maternal ICD9 
      linkedB %in% c("Y", "M"), 
      
      # spontaneous PTB WITH complications
      is.na(prem_5cat_spon_nocomp),
      !is.na(prem_5cat_spon)
      
      ) %>% 
    
    return()
}



  


  