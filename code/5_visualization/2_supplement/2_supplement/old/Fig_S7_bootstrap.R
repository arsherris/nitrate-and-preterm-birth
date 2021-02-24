source('E:/Projects/WaterExposure/R/1_Nitrate_PTB_siblings/code/6_visualization/2_supplement/0_fig_compare_results_fxn.R', echo=TRUE)

boot_early <- read_csv("data_out/results/sensitivity/2_model_spec/bootstrap_ses/Nitrate_boot_tbl_early_2020-09-02.csv")
boot_late <- read_csv("data_out/results/sensitivity/2_model_spec/bootstrap_ses/Nitrate_boot_tbl_late_2020-09-02.csv")
boot_comb <- rbind(boot_early, boot_late)

res_primary <- read_csv("data_out/results/primary/Nitrate_res_primary_consec_2020-08-30.csv") %>% 
  filter(effect == "Main") %>% 
  mutate(model = "Wald")

res_boot <- boot_comb %>% 
  left_join(select(res_primary, outcome, exposure_cat, OR), 
            by = c("outcome", "exposure_cat")) %>% 
  mutate(model = "Bootstrap") %>% 
  bind_rows(res_primary)
  

# save

plot <- plot_model_compare(res_boot,
                   "Method for estimating \nconfidence intervals")

plot

ggsave("data_out/visualizations/2_supplement/Fig_S7_bootstrap.png",
       plot, device = "png", width = 7.5, dpi = 150, height = 4, units = "in")


