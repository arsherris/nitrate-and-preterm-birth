# Sensitivity: Effect of different models and specifications

source('code/6_visualization/2_supplement/0_fig_compare_results_fxn.R')

# load results from different models and standardize column names

  results_carryover <- read_csv("data_out/results/secondary/3_carryover/Nitrate_res_glmer_adj_2020-08-19.csv") %>% 
    mutate(model = effect) %>% 
    select(outcome, exposure_cat,  OR, lower, upper, model)


  # plot model copmarison
  plot <- plot_model_compare(results_carryover, "Effect")
  plot
  
  ggsave("data_out/visualizations/2_supplement/Fig_S4_carryover.png",
         plot, device = "png", width = 7.5, dpi = 150, height = 4, units = "in")
  
    
