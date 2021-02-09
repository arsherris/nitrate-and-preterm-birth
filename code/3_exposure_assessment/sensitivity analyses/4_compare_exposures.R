exposure_sensitivity <- births_crossover_exposure %>% 
  left_join(select(sibs_crossover_simple, birth_id, exp_cat), by = "birth_id",
            suffix = c("", "_simple")) %>% 
  left_join(select(sibs_crossover_low, birth_id, exp_cat), by = "birth_id",
            suffix = c("_orig", "_low")) 

table(exposure_sensitivity$exp_cat_simple)
table(exposure_sensitivity$exp_cat_low)

table(exposure_sensitivity$exp_cat_orig)
table(exposure_sensitivity$exp_cat_orig, exposure_sensitivity$exp_cat_simple)
table(exposure_sensitivity$exp_cat_orig, exposure_sensitivity$exp_cat_low)

