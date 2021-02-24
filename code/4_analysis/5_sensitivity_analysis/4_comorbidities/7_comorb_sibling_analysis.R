load("data/processed/births_exposure/siblings_comorb.RData")

# find exposure-discordant siblings in crossover sample
# siblings_comorb <- siblings_comorb %>% 
#   mutate(parity_cat = case_when(parity < 10 ~ parity,
#                                 parity >= 10 ~ 10))

ids_high <- unique(siblings_comorb$mat_id[siblings_comorb$exp_cat == "high"])
ids_med <- unique(siblings_comorb$mat_id[siblings_comorb$exp_cat == "medium"])
ids_low <- unique(siblings_comorb$mat_id[siblings_comorb$exp_cat == "low"])

sibs_comorb_discordant <- siblings_comorb %>% 
  filter(mat_id %in% c(ids_high[ids_high %in% ids_low],
                       ids_high[ids_high %in% ids_med],
                       ids_med[ids_med %in% ids_low]))

# results tables

res_sibs_comorb <- res_table_primary(sibs_comorb_discordant, clog_adj)

# save

write_output(res_sibs_comorb, "output/results/sensitivity/4_comorbidities/")




# continuous
mod_continuous_early <- clog_adj_conc(siblings_comorb, "prem_20_to_31")
mod_continuous_late <- clog_adj_conc(siblings_comorb, "prem_32_to_36")

summary(mod_continuous_early)
summary(mod_continuous_late)
save(mod_continuous_early, mod_continuous_late, file = "output/results/sensitivity/1_exposure_assumption/continuous/mods_continuous.RData")
load("output/results/sensitivity/1_exposure_assumption/continuous/mods_continuous.RData")

# find sample size for continuous analysis
table(mod_continuous_early$y[,2])
table(mod_continuous_late$y[,2])

# log

mod_continuous_early_log <-  clog_adj_conc_log(siblings_comorb, "prem_20_to_31")
mod_continuous_early_full <- clog_adj_conc_full(siblings_comorb, "prem_20_to_31")

save(mod_continuous_early_log, mod_continuous_early_full, file = "output/results/sensitivity/1_exposure_assumption/continuous/mods_continuous_early_compare.RData")


mod_continuous_late_log <-  clog_adj_conc_log(siblings_comorb, "prem_32_to_36")
mod_continuous_late_full <- clog_adj_conc_full(siblings_comorb, "prem_32_to_36")


summary(mod_continuous_early_log)
Sys.time()
summary(mod_continuous_late)
save(mod_continuous_early, mod_continuous_late, file = "output/results/sensitivity/1_exposure_assumption/continuous/mods_continuous.RData")


# try log transformed?



## only sibs with sets excluding late PTB
View(filter(sibs_discordant, mat_id %in% mat_id[is.na(mat_age_5cat)]))
sibs_discordant_early <- sibs_discordant %>% 
  filter(!is.na(mat_age),
         !is.na(prem_20_to_31))# %>% 
  # group_by(mat_id) %>% 
  # filter(n() > 1) %>% 
  # ungroup

mod_primary_early_test <- clog_adj(sibs_discordant_early, outcome = "prem_20_to_31")






# all sibs (including non-consec)

# find exposure-discordant siblings in crossover sample

ids_high <- unique(sibs_all$mat_id[sibs_all$exp_cat == "high"])
ids_med <- unique(sibs_all$mat_id[sibs_all$exp_cat == "medium"])
ids_low <- unique(sibs_all$mat_id[sibs_all$exp_cat == "low"])

sibs_discordant <- sibs_all %>% 
  filter(mat_id %in% c(ids_high[ids_high %in% ids_low],
                       ids_high[ids_high %in% ids_med],
                       ids_med[ids_med %in% ids_low]))

save(sibs_discordant, file ="data/processed/births_exposure/sibs_discordant_all.RData")

# results tables
res_primary_exact_all <- res_table_primary(sibs_discordant, clog_adj)

# save
write_output(res_primary_exact_all, "output/results/primary/")


# results tables
res_primary_full_all <- res_table_primary(sibs_discordant, clog_adj_full)

# save
write_output(res_primary_full_all, "output/results/primary/")


## siblings only prem discordant

# early ptb
sibs_disc_early <- sibs_discordant %>% 
  filter(!is.na(prem_20_to_31)) %>% 
  filter(mat_id %in% mat_id[duplicated(mat_id)])

res_early_disc <- model_output_table(sibs_disc_early, clog_adj, "prem_20_to_31")

# late ptb
sibs_disc_late <- sibs_discordant %>% 
  filter(!is.na(prem_32_to_36)) %>% 
  filter(mat_id %in% mat_id[duplicated(mat_id)])

res_late_disc <- model_output_table(sibs_disc_late, clog_adj, "prem_32_to_36")

save(res_early_disc, res_late_disc, file = "output/results/primary/separate_outcome_res.RData")
