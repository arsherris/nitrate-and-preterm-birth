## SECONDARY ANALYSIS: INDIVIDUAL-LEVEL (CASE-CONTROL) ANALYSIS 
## Nitrate in drinking water and spontaneous preterm birth
## Author: A. Sherris

source("code/0_config.R")
source("code/3_analysis/1_define_model_fxns/2_glmer_fxns.R")
source("code/3_analysis/1_define_model_fxns/3_results_table_fxns.R")

load("data/processed/births_exposure/births_case_control.RData")

# run models

mod_case_control_early <- glmer_case_control_adj(births_case_control, "prem_20_to_31")
mod_case_control_late <- glmer_case_control_adj(births_case_control, "prem_32_to_36")

# generate results table

res_case_control_all_births <- bind_rows(clean_model_output(mod_case_control_early, "prem_20_to_31"),
                                         clean_model_output(mod_case_control_late, "prem_32_to_36"))

write_output(res_case_control_all_births, "output/results/secondary/2_cohort/")

# end