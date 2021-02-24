## PRIMARY ANALYSIS: SIBLING SAMPLE
## Nitrate in drinking water and spontaneous preterm birth
## Author: A. Sherris

load("data/processed/births_exposure/sibs_consecutive.RData")

# find exposure-discordant siblings in crossover sample

ids_high <- unique(sibs_consecutive$mat_id[sibs_consecutive$exp_cat == "high"])
ids_med <- unique(sibs_consecutive$mat_id[sibs_consecutive$exp_cat == "medium"])
ids_low <- unique(sibs_consecutive$mat_id[sibs_consecutive$exp_cat == "low"])

sibs_discordant <- sibs_consecutive %>% 
  filter(mat_id %in% c(ids_high[ids_high %in% ids_low],
                       ids_high[ids_high %in% ids_med],
                       ids_med[ids_med %in% ids_low]))

# run models

mod_siblings_early <- clog_adj(sibs_discordant, "prem_20_to_31")
mod_siblings_late  <- clog_adj(sibs_discordant, "prem_32_to_36")

save(mod_siblings_early, mod_siblings_late, file = "output/results/primary/sibling_models.RData")

# generate results table

res_sibling_analysis <- bind_rows(clean_model_output(mod_siblings_early, "prem_20_to_31"),
                                  clean_model_output(mod_siblings_late,  "prem_32_to_36"))

write_output(res_sibling_analysis, "output/results/primary/")

# end