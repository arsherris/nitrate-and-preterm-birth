## split by parity

# split
data_parity <- split(births_case_control, births_case_control$parity_cat)

# results by parity
res_parity_case_control <- map_df(data_parity, 
                  function(x) res_table_primary(x, glmer_case_control_adj_noparity),
                  .id = "parity") 

# save
write_output(res_parity_case_control, "output/results/sensitivity/3_stratification/parity/")

# interaction

mod_parity_early <-  glmer(prem_20_to_31 ~ exp_cat +
                          factor(parity_cat)*exp_cat + 
                    payer + precare +
                    mat_race + mat_educ + mat_age_5cat +
                    (1|pwsid) + (1|mat_id),
                  family = binomial, 
                  control = fast_control,
                  nAGQ = 0,
                  data = births_case_control)

mod_parity_late <-  glmer(prem_32_to_36 ~ exp_cat +
                             factor(parity_cat)*exp_cat + 
                             payer + precare +
                             mat_race + mat_educ + mat_age_5cat +
                             (1|pwsid) + (1|mat_id),
                           family = binomial, 
                           control = fast_control,
                           nAGQ = 0,
                           data = births_case_control)


summary(mod_parity_early)
summary(mod_parity_late)

save(mod_parity_early, mod_parity_late, file = "output/results/sensitivity/3_stratification/parity/interaction_mods.RData")

# interaction with function

res_interaction_parity <- res_table_interactions(births_case_control, glmer_interaction, "parity")
write_output(res_interaction_parity, "output/results/sensitivity/3_stratification/infant_parity/")

# compare model with and without interaction

mod_case_control_early <- glmer_case_control_adj(births_case_control, "prem_20_to_31")
mod_case_control_late  <- glmer_case_control_adj(births_case_control, "prem_32_to_36")

save(mod_case_control_early, mod_case_control_late, file = "output/results/secondary/2_cohort/case_control_mods.RData")

lrtest(mod_parity_early, mod_case_control_early)
lrtest(mod_parity_late, mod_case_control_late)
