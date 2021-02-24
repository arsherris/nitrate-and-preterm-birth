# compare siblings who don't move to all siblings in the crossover sample

res_nomove <- read_csv("data_out/results/secondary/1_movement/Nitrate_res_same_pws_consec_2020-08-30.csv") %>% 
  filter(effect == "Main") %>% 
  mutate(model = "Siblings born in the \nsame water system")

res_primary <- read_csv("data_out/results/primary/Nitrate_res_primary_consec_2020-08-30.csv") %>% 
  filter(effect == "Main") %>% 
  mutate(model = "All siblings")


res_compare_moves <- rbind(res_primary, res_nomove) 

plot <- plot_model_compare(res_compare_moves, "Sample")

plot

ggsave("data_out/visualizations/2_supplement/Fig_S2_sib_moves.png",
       plot, device = "png", width = 7.5, dpi = 150, height = 4, units = "in")
