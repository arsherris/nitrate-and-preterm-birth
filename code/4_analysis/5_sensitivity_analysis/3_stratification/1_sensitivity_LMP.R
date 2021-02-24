# separate the study population by kind of gestational age estimate

births_case_control_lmp <- births_case_control %>% 
  filter(year < 2007)

births_case_control_boe <- births_case_control %>% 
  filter(year >= 2007)

# run primary model

results_lmp <- res_table_primary(births_case_control_lmp, glmer_case_control_adj) %>% 
  mutate(method = "LMP")
results_boe <- res_table_primary(births_case_control_boe, glmer_case_control_adj) %>% 
  mutate(method = "BOE")

results_lmp_boe <- rbind(results_lmp, results_boe) 

# export

write_output(results_lmp_boe, "output/results/sensitivity/3_stratification/lmp_boe/")

# interaction term

births_case_control$gestage_method <- ifelse(births_case_control$year < 2007, "LMP", "BOE")

mod_lmp_early <- glmer_interaction(births_case_control, "gestage_method", "prem_20_to_31")
mod_lmp_late <- glmer_interaction(births_case_control, "gestage_method", "prem_32_to_36")


res_interaction_lmp <- res_table_interactions(births_case_control, glmer_interaction, "LMP/BOE")
write_output(res_interaction_lmp, "output/results/sensitivity/3_stratification/lmp_boe")

# models

births_case_control <- births_case_control %>% 
  mutate(time_period = case_when(year < 2007 ~ "LMP",
                                  year >= 2007 ~ "BOE"))

mod_time_period_early <-  glmer(prem_20_to_31 ~ exp_cat +
                               time_period +
                               factor(parity_cat) +
                               payer + precare +
                               mat_race + mat_educ + mat_age_5cat +
                               (1|pwsid) + (1|mat_id),
                             family = binomial, 
                             control = fast_control,
                             nAGQ = 0,
                             data = births_case_control)

mod_time_period_late <-  glmer(prem_32_to_36 ~ exp_cat +
                              time_period +
                              factor(parity_cat) +
                              payer + precare +
                              mat_race + mat_educ + mat_age_5cat +
                              (1|pwsid) + (1|mat_id),
                            family = binomial, 
                            control = fast_control,
                            nAGQ = 0,
                            data = births_case_control)


mod_time_period_early_int <- update(mod_time_period_early, . ~ . + exp_cat*time_period)
mod_time_period_late_int  <- update(mod_time_period_late,  . ~ . + exp_cat*time_period)

save(mod_time_period_early, mod_time_period_late, mod_time_period_early_int, mod_time_period_late_int,
     file = "output/results/sensitivity/3_stratification/lmp_boe/time_period_mods.RData")


lrtest(mod_time_period_early_int, mod_time_period_early)
lrtest(mod_time_period_late_int, mod_time_period_late)

