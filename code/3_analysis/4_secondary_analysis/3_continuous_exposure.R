## SECONDARY ANALYSIS: CONTINUOUS EXPOSURE 
## Nitrate in drinking water and spontaneous preterm birth
## Author: A. Sherris

load("data/processed/births_exposure/sibs_consecutive.RData")

## sibling sample -------------------------------------------------------------------------

  # run models
  
  mod_sibs_early_conc <- clog_adj_conc(sibs_consecutive, "prem_20_to_31")
  mod_sibs_late_conc  <- clog_adj_conc(sibs_consecutive, "prem_32_to_36")

  # generate results table
  
  res_sibs_continuous <- bind_rows(clean_model_output_conc(mod_sibs_early_conc, "prem_20_to_31"),
                                   clean_model_output_conc(mod_sibs_late_conc, "prem_32_to_36")) 
  
  write_output(res_sibs_continuous, "output/results/primary/")

## sibling sample without maternal movement --------------------------------------------------
  
  # run models
  
  mod_same_pws_early_conc <- clog_adj_conc(sibs_same_pws, "prem_20_to_31")
  mod_same_pws_late_conc  <- clog_adj_conc(sibs_same_pws, "prem_32_to_36")
  
  # generate results table
  
  res_same_pws_continuous <- bind_rows(clean_model_output_conc(mod_same_pws_early_conc, "prem_20_to_31"),
                                       clean_model_output_conc(mod_same_pws_late_conc, "prem_32_to_36")) 
  
  write_output(res_same_pws_continuous, "output/results/secondary/1_movement/")
  
  
## individual-level sample ------------------------------------------------------------------

  # run models
  
  mod_case_control_early_conc <- glmer_case_control_conc(births_case_control, "prem_20_to_31")
  mod_case_control_late_conc  <- glmer_case_control_conc(births_case_control, "prem_32_to_36")
  
  # generate results table
  
  res_case_control_conc <- bind_rows(clean_model_output_conc(mod_case_control_early_conc, "prem_20_to_31"),
                                     clean_model_output_conc(mod_case_control_late_conc, "prem_32_to_36"))
  
  write_output(res_case_control_continuous, "output/results/secondary/2_cohort/")


# end