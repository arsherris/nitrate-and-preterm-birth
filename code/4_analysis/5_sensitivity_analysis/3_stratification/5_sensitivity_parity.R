# sibling sample stratified by parity

sibs_p1_p2 <- sibs_consecutive %>% 
  filter(mat_id %in% mat_id[parity == 1],
         mat_id %in% mat_id[parity == 2],
         parity %in% 1:2)

sibs_p2_p3 <- sibs_consecutive %>% 
  filter(mat_id %in% mat_id[parity == 2],
         mat_id %in% mat_id[parity == 3],
         parity %in% 2:3)

ids_high <- unique(sibs_p1_p2$mat_id[sibs_p1_p2$exp_cat == "high"])
ids_med <- unique(sibs_p1_p2$mat_id[sibs_p1_p2$exp_cat == "medium"])
ids_low <- unique(sibs_p1_p2$mat_id[sibs_p1_p2$exp_cat == "low"])

sibs_p1_p2_disc <- sibs_p1_p2 %>% 
  filter(mat_id %in% c(mat_id[ids_high %in% ids_low],
                       ids_high[ids_high %in% ids_med],
                       ids_med[ids_med %in% ids_low]))


ids_high <- unique(sibs_p2_p3$mat_id[sibs_p2_p3$exp_cat == "high"])
ids_med <- unique(sibs_p2_p3$mat_id[sibs_p2_p3$exp_cat == "medium"])
ids_low <- unique(sibs_p2_p3$mat_id[sibs_p2_p3$exp_cat == "low"])

sibs_p2_p3_disc <- sibs_p2_p3 %>% 
  filter(mat_id %in% c(mat_id[ids_high %in% ids_low],
                       ids_high[ids_high %in% ids_med],
                       ids_med[ids_med %in% ids_low]))

# results tables
res_primary_p1_p2 <- res_table_primary(sibs_p1_p2_disc, clog_adj)
res_primary_p2_p3 <- res_table_primary(sibs_p2_p3_disc, clog_adj)

# save
write_output(res_primary_p1_p2, "output/results/secondary/")
write_output(res_primary_p2_p3, "output/results/secondary/")



save(mod_primary_early, mod_pr)
