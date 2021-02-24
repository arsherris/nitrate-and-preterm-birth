## split by sex

# split
data_sex <- split(births_case_control, births_case_control$sex)

# results by sex
res_sex_case_control <- map_df(data_sex, 
                  function(x) res_table_primary(x, glmer_case_control_adj),
                  .id = "Sex") 

# save
write_output(res_sex_case_control, "output/results/sensitivity/3_stratification/infant_sex/")

# interaction

load("output/results/secondary/2_cohort/case_control_mods.RData")

mod_sex_early <- update(mod_case_control_early, . ~ . + sex)
mod_sex_late  <- update(mod_case_control_late,  . ~ . + sex)

mod_sex_early_int <- update(mod_sex_early, . ~ . + exp_cat*sex)
mod_sex_late_int  <- update(mod_sex_late,  . ~ . + exp_cat*sex)

lrtest(mod_sex_early_int, mod_sex_early)
lrtest(mod_sex_late_int, mod_sex_late)

save(mod_sex_early, mod_sex_late, mod_sex_early_int, mod_sex_late, 
     file = "output/results/sensitivity/3_stratification/infant_sex/sex_int_mods.RData")
# 
# # interaction with function
# 
# res_interaction_sex <- res_table_interactions(births_case_control, glmer_interaction, "sex")
# write_output(res_interaction_sex, "output/results/sensitivity/3_stratification/infant_sex/")
