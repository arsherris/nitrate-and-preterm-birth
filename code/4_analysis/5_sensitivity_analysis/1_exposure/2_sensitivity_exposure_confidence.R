#load("data_processed/births_exposure/births_case_control_2020-07-16.RData")


births_certainty <- births_case_control %>% 
  mutate(certainty = case_when(
    interval == "long" ~ 1,
    source_n < 4 ~ 1,
    source_n < 50 ~ 2,
    TRUE ~ 1)) 

# split
births_certainty <- split(births_certainty, births_certainty$certainty)

# results by sex
res_certainty <- map_df(births_certainty, 
                        function(x) res_table_primary(x, glmer_case_control_adj),
                        .id = "certainty")  %>% 
  mutate(model = ifelse(certainty == 1, "More uncertain", "Less uncertain"))

write_output(res_certainty, "output/results/sensitivity/1_exposure_assumption/exp_certainty/")

# compare model with and without interaction

births_certainty <- births_case_control %>% 
  mutate(certainty = case_when(
    interval == "long" ~ 1,
    source_n < 4 ~ 1,
    source_n < 50 ~ 2,
    TRUE ~ 1)) 

mod_certainty_early <-  glmer(prem_20_to_31 ~ exp_cat +
                                certainty +
                             factor(parity_cat) +
                             payer + precare +
                             mat_race + mat_educ + mat_age_5cat +
                             (1|pwsid) + (1|mat_id),
                           family = binomial, 
                           control = fast_control,
                           nAGQ = 0,
                           data = births_certainty)

mod_certainty_late <-  glmer(prem_32_to_36 ~ exp_cat +
                            certainty +
                            factor(parity_cat) +
                            payer + precare +
                            mat_race + mat_educ + mat_age_5cat +
                            (1|pwsid) + (1|mat_id),
                          family = binomial, 
                          control = fast_control,
                          nAGQ = 0,
                          data = births_certainty)

mod_certainty_early_int <- update(mod_certainty_early, . ~ . + exp_cat*certainty)
mod_certainty_late_int  <- update(mod_certainty_late,  . ~ . + exp_cat*certainty)

save(mod_certainty_early, mod_certainty_late, mod_certainty_early_int, mod_certainty_late_int,
     file = "output/results/sensitivity/1_exposure_assumptions/exp_certainty/certainty_mods.RData")

lrtest(mod_certainty_early, mod_certainty_early_int)
lrtest(mod_certainty_late, mod_certainty_late_int)

