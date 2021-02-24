source('E:/Projects/WaterExposure/R/1_Nitrate_PTB_siblings/code/6_visualization/2_supplement/0_fig_compare_results_fxn.R', echo=TRUE)

# assumption: sourcse contribute less if conc over mcl

  results_mcl <- read_csv("data_out/results/sensitivity/1_exposure_assumption/exp_certainty/") %>% 
    filter(effect == "Main") %>% 
    mutate(model = "Sensitivity analysis")
  
  results_orig = read_csv("data_out/results/primary/Nitrate_res_glmer_adj_2020-08-19.csv") %>% 
    filter(effect == "Main") %>% 
    mutate(model = "Original")
  
  results_exp_assess <- rbind(results_mcl, results_orig)
  
  plot_model_compare(results_exp_assess,  "Assumption")
  
  ggsave("data_out/4_visualizations/2_supplement/compare_lmp_boe.png")

## compare exposure assignments
  
load("data_processed/births_exposure/sensitivity/sibs_crossover_mcl.RData")  
load("data_processed/births_exposure/nitrate_births_crossover_timeweight_2020-07-18.RData")  

table(births_crossover_exposure$exp_cat, sibs_crossover_mcl$exp_cat, useNA = "a")

sum(c(864357, 80454, 2649))/nrow(sibs_crossover_mcl)

## 97% same exposure


