source('E:/Projects/WaterExposure/R/1_Nitrate_PTB_siblings/code/6_visualization/2_supplement/0_fig_compare_results_fxn.R', echo=TRUE)

res_certainty <- read_csv("data_out/results/sensitivity/1_exposure_assumption/exp_certainty/Nitrate_res_certainty_2020-08-24.csv")
# save
write_output(res_certainty, "data_out/results/sensitivity/1_exposure_assumption/exp_certainty/")

plot <- plot_model_compare(res_certainty,
                   "Degree of uncertainty in \nexposure assessment")

plot
ggsave("data_out/visualizations/2_supplement/Fig_S6_certainty.png",
       plot, device = "png", width = 7.5, dpi = 150, height = 4, units = "in")


