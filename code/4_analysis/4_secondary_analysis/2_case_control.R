## SECONDARY ANALYSIS: INDIVIDUAL-LEVEL (CASE-CONTROL) ANALYSIS 
## Nitrate in drinking water and spontaneous preterm birth
## Author: A. Sherris

load("data/processed/births_exposure/births_case_control.RData")

# run models

mod_case_control_early <- glmer_case_control_adj(births_case_control, "prem_20_to_31")
mod_case_control_late <- glmer_case_control_adj(births_case_control, "prem_32_to_36")

save(mod_case_control_early, mod_case_control_late, "output/results/secondary/2_cohort/cohort_mods.RData")

# generate results table

res_case_control_all_births <- bind_rows(clean_model_output(mod_case_control_early, "prem_20_to_31"),
                                         clean_model_output(mod_case_control_late, "prem_32_to_36"))

write_output(res_case_control_all_births, "output/results/secondary/2_cohort/")

# end