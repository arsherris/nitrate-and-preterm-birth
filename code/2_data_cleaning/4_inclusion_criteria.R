## DEFINE INCLUSION CRITERIA
## Nitrate in drinking water and spontaneous preterm birth
## Author: A. Sherris

# function: apply exclusion criteria to births data
  # input: cleaned births data
  # output: cleaned births data restricted to study population

inclusion_study_pop <-  function(data_births_cleaned) {
  
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
      
      # geocode within CA boundary
      !is.na(county),
      
      # geocode to state or zip code
      MATCHED != "City",
      
      # better link between OSHPD and birth certificate
      link == 1,
      
      #  bettter match with VSB, PDDI, and PDDM for maternal ICD9 
      linkedB %in% c("Y", "M"), 
      
      # spontaneous PTB no complications
      !is.na(prem_5cat_spon_nocomp)
      
      ) %>% 
    
    return()
}



  


  