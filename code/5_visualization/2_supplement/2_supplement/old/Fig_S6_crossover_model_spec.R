## compare effect of model adjustments on results

# load model comparison function
source('code/6_visualization/2_supplement/0_fig_compare_results_fxn.R')


# load results tables

res_unadj <- read_csv("data_out/results/sensitivity/2_model_spec/model_spec/Nitrate_res_unadj_c_2020-08-26.csv") %>% 
  mutate(model = "1. Unadjusted conditional logistic regression")
res_adj <- read_csv("data_out/results/primary/Nitrate_res_primary_consec_2020-08-30.csv") %>% 
  mutate(model = "2. Adjusted for maternal age, parity, and short IPI (primary analysis)")
res_adj_yr <- read_csv("data_out/results/sensitivity/2_model_spec/model_spec/Nitrate_res_yr_c_2020-08-26.csv") %>% 
  mutate(model = "3. Additionally adjusted for year")
res_adj_yr_mo <- read_csv("data_out/results/sensitivity/2_model_spec/model_spec/Nitrate_res_yr_mo_doc_c_2020-08-26.csv") %>% 
  mutate(model = "4. Additionally adjusted for month of conception")
res_adj_region <- read_csv("data_out/results/sensitivity/2_model_spec/model_spec/Nitrate_res_region_c_2020-08-27.csv") %>% 
  mutate(model = "5. Additionally adjusted for region")

# combine results tables
crossover_models <- bind_rows(res_unadj, res_adj, res_adj_yr,
                              res_adj_yr_mo, res_adj_region) %>% 
  filter(effect == "Main")


# plot model copmarison
plot <- plot_model_compare(crossover_models, "Model")

plot

ggsave("data_out/visualizations/2_supplement/Fig_S6_model_specA.png",
       plot, device = "png", width = 8.3, dpi = 150, height = 4, units = "in")


plot <- plot_model_compare(crossover_models, "Model", legend_position = "none")

plot

ggsave("data_out/visualizations/2_supplement/Fig_S6_model_specB.png",
       plot, device = "png", width = 6, dpi = 150, height = 4, units = "in")
