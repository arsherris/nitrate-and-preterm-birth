## compare effect of model adjustments on results

# load results tables

res_primary <- read_csv("data_out/results/primary/Nitrate_res_primary_consec_2020-08-30.csv") %>% 
  mutate(model = "Sibling sample") %>% 
  filter(effect == "Main")

res_case_control_all_births <- read_csv("data_out/results/secondary/2_case_control/Nitrate_res_case_control_all_births_2020-08-23.csv") %>% 
  mutate(model = "Case control sample \n(all births)")

res_case_control_p1_births <- read_csv("data_out/results/secondary/2_case_control/Nitrate_res_case_control_p1_births_2020-08-23.csv") %>% 
  mutate(model = "Case control sample \n(first-parity births)")


# combine results tables
secondary_models <- bind_rows(res_case_control_all_births, res_case_control_p1_births) 


# plot model copmarison
plot <- plot_model_compare(secondary_models, "Sample")
plot

ggsave("data_out/visualizations/2_supplement/Fig_S3_case_control.png",
       plot, device = "png", width = 7.5, dpi = 150, height = 4, units = "in")
