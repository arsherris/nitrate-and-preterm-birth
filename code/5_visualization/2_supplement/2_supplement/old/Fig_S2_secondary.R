# compare siblings who don't move to all siblings in the crossover sample
source("code/6_visualization/2_supplement/0_fig_compare_results_fxn.R")

res_primary_mod <- read_csv("data_out/results/primary/Nitrate_res_primary_consec_2020-08-30.csv") %>% 
  filter(effect == "Main") %>% 
  mutate(model = "Sibling sample")

res_secondary_no_move <- read_csv("data_out/results/secondary/1_movement/Nitrate_res_same_pws_consec_2020-08-30.csv") %>% 
  filter(effect == "Main") %>% 
  mutate(model = "Siblings born in the \nsame water system")

res_secondary_first_parity <- read_csv("data_out/results/sensitivity/2_model_spec/first_parity_crossover_samp/Nitrate_res_primary_crossover_2020-08-30.csv")

res_secondary_case_control <- read_csv("data_out/results/secondary/2_case_control/Nitrate_res_case_control_all_births_2020-08-23.csv") %>% 
  filter(effect == "Main") %>% 
  mutate(model = "Case-control sample")

res_secondary_p1 <- read_csv("data_out/results/secondary/2_case_control/Nitrate_res_case_control_p1_births_2020-08-23.csv") %>% 
  filter(effect == "Main") %>% 
  mutate(model = "First-parity births")


res_secondary <- rbind(res_primary_mod, res_secondary_no_move, res_secondary_case_control, res_secondary_p1) 
write_output(res_secondary, "data_out/results/secondary/")

plot <- plot_model_compare(res_secondary, "Sample")

plot

ggsave("data_out/visualizations/2_supplement/Fig_S3_sib_moves.png",
       plot, device = "png", width = 7.5, dpi = 150, height = 4, units = "in")
