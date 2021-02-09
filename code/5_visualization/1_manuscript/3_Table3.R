## GENERATE TABLE 3
## Nitrate in drinking water and spontaneous preterm birth
## Author: A. Sherris

## Generate Table 3: Primary and secondary results table

# load results tables
res_sibs <- read_csv("output/results/primary/Nitrate_res_sibling_analysis_2020-12-30.csv") %>% 
  mutate(model = "All Siblings")
res_sibs_continuous <- read_csv("output/results/primary/Nitrate_res_sibs_continuous_2021-02-05.csv") %>% 
  mutate(model = "All Siblings", exposure_cat = "Continuous")

res_same_pws <- read_csv("output/results/secondary/1_movement/Nitrate_res_same_pws_2020-12-21.csv") %>% 
  mutate(model = "Same PWS")
res_same_pws_continuous <- read_csv("output/results/secondary/1_movement/Nitrate_res_same_pws_continuous_2020-12-21.csv")%>% 
  mutate(model = "Same PWS", exposure_cat = "Continuous")

res_case_control <- read_csv("output/results/secondary/2_cohort/Nitrate_res_case_control_all_births_2020-12-16.csv") %>% 
  mutate(model = "Individual-level")
res_case_control_continuous <- read_csv("output/results/secondary/2_cohort/Nitrate_res_case_control_continuous_2020-12-16.csv")%>% 
  mutate(model = "Individual-level", exposure_cat = "Continuous")

# join results tables
table3 <- bind_rows(res_sibs, res_sibs_continuous,
                     res_same_pws, res_same_pws_continuous,
                     res_case_control, res_case_control_continuous) %>% 
  select(model, outcome, exposure_cat, n_term, n_preterm, result) %>% 
  arrange(outcome)

write_output(table3, "output/visualizations/1_manuscript/")

# END
