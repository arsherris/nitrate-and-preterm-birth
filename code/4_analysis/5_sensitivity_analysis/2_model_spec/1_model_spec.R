
# results tables for crossover analysis - adjusted and unadjusted mixed effects logistic regression

source("/code/5_analysis/0_model_fxns/4_run_analysis_fxn.R")

  # unadjusted models

  res_unadj <- res_table_primary(data = sibs_crossover_med, mod_fxn = clog_unadj)

 
  # adjusted - year and month
  res_yr <- res_table_primary(data = sibs_crossover_med, mod_fxn = clog_adj_yr)
  res_yr_mo <- res_table_primary(data = sibs_crossover_med, mod_fxn = clog_adj_month)
  res_region <- res_table_primary(data = sibs_crossover_med, clog_adj_region)

# save tables
  
  write_output(res_unadj, "output/results/sensitivity/2_model_spec/model_spec/")
  write_output(res_yr, "output/results/sensitivity/2_model_spec/model_spec/")
  write_output(res_yr_mo, "output/results/sensitivity/2_model_spec/model_spec/")
  
  
