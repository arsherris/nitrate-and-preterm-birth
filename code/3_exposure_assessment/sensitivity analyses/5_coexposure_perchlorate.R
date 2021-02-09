births_perchlorate <- births_case_control

load("data_processed/births_exposure/nitrate_births_case_control_2020-07-16.RData")
births_nitrate <- births_case_control

load("data_processed/births_exposure/nitrate_births_crossover_timeweight_2020-07-18.RData")
sibs_nitrate <- births_crossover_exposure
  
births_compare <- births_nitrate %>% 
  left_join(select(births_perchlorate, birth_id, exp_cat), by = "birth_id", suffix = c("_no3", "_per"))

table(births_compare$exp_cat_no3, births_compare$exp_cat_per, useNA = "a")

sibs_compare <- sibs_nitrate %>% 
  left_join(select(births_perchlorate, birth_id, exp_cat), by = "birth_id", suffix = c("_no3", "_per"))

table(sibs_compare$exp_cat_no3, sibs_compare$exp_cat_per, useNA = "a")

# restrict case control population to those without high perchlorate
births_no_perc <- births_compare %>% 
  filter(exp_cat_per != "high") %>% 
  rename(exp_cat = exp_cat_no3,
         pwsid = pwsid.x)

res_case_control_no_perc <- res_table_primary(births_no_perc, glmer_case_control_adj)

write_output(res_case_control_no_perc, "data_out/3_results/sensitivity/perchlorate_coexposure")
