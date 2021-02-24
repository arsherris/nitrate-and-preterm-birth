## LOAD CLEAN DATA
## Drinking water and PTB sybling analysis
## Author: A. Sherris

# water quality
  load("data/processed/clean_input/nitrate_wq_data.RData")
  
# births
  
  # study population
  load("data/processed/clean_input/births_study_pop.RData")
  
  # case-control sample
  load("data/processed/births_exposure/births_case_control.RData")

# sibling sample
  
  # all siblings
  load("data/processed/births_exposure/sibs_consecutive.RData")

# end
  
  
  
  