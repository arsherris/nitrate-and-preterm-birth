source('code/6_visualization/1_manuscript/table_functions.R')

## Descriptive stats of women excluded 

  # load data
 load("data/processed/clean_input/births_study_pop.RData")  
 load("data/processed/maternal_pwsid/births_study_pop_pws_final.RData")
 load("data/processed/births_exposure/births_case_control.RData")
 
 # identify births with unassigned PWS and nitrate
 births_study_pop_tbl <- prep_tbl_data(births_study_pop)
 
 births_no_pws <- births_study_pop_pws_final %>% 
    filter(pws_type == "unassigned") %>% 
    prep_tbl_data
  
  births_no_nitrate <- births_study_pop_pws_final %>% 
    filter(pws_type != "unassigned",
           !birth_id %in% births_case_control$birth_id) %>% 
    prep_tbl_data
 
  births_case_control_tbl <- prep_tbl_data(births_case_control) 

 # generate descriptive table for each sample
  
  options(scipen = 999)
  
  table1_study_pop    <- tab_descriptive(births_study_pop_tbl)
  table1_no_pws       <- tab_descriptive(births_no_pws)
  table1_no_nitrate   <- tab_descriptive(births_no_nitrate)
  table1_case_control <- tab_descriptive(births_case_control_tbl)
  
  # merge tables
  
  
  table_S1 <- table1_study_pop %>% 
    select(variable, value, study_pop = percent) %>% 
    left_join(select(table1_no_pws, variable, value, no_pws = percent), 
              by = c("variable", "value")) %>% 
    left_join(select(table1_no_nitrate, variable, value, no_nitrate = percent), 
              by = c("variable", "value")) %>% 
    left_join(select(table1_case_control, variable, value, case_control = percent), 
              by = c("variable", "value"))
  
  write_output(table_S1, "output/visualizations/2_supplement/") 
  
  
  