## SECONDARY ANALYSIS: SIBLINGS WITHOUT MATERNAL MOVEMENT
## Nitrate in drinking water and spontaneous preterm birth
## Author: A. Sherris

# find discordant siblings without maternal movement

load("data/processed/births_exposure/sibs_consec_same_pws.RData")

ids_high <- unique(sibs_same_pws$mat_id[sibs_same_pws$exp_cat == "high"])
ids_med <- unique(sibs_same_pws$mat_id[sibs_same_pws$exp_cat == "medium"])
ids_low <- unique(sibs_same_pws$mat_id[sibs_same_pws$exp_cat == "low"])

ids_discordant <- unique(c(ids_high[ids_high %in% ids_low],
                           ids_high[ids_high %in% ids_med],
                           ids_med[ids_med %in% ids_low]))

sibs_same_pws_disc <- sibs_same_pws %>% 
  filter(mat_id %in% ids_discordant)

# run models
  
  mod_same_pws_early <- clog_adj(sibs_same_pws_disc, "prem_20_to_31")
  mod_same_pws_late  <- clog_adj(sibs_same_pws_disc, "prem_32_to_36")
  
  save(mod_same_pws_early, mod_same_pws_late, file = "output/results/primary/sibling_models.RData")
  
# generate results table
  
  res_same_pws <- bind_rows(clean_model_output(mod_same_pws_early, "prem_20_to_31"),
                            clean_model_output(mod_same_pws_late,  "prem_32_to_36"))
  
  write_output(res_same_pws, "output/results/secondary/1_movement/")
  
# end
  