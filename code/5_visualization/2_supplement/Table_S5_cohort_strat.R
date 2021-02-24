## compare effect of model adjustments on results

res_cohort_all_births <- read_csv("output/results/secondary/2_case_control/Nitrate_res_case_control_all_births_2020-08-23.csv") %>% 
  mutate(model = "Full cohort")

res_p1_births <- read_csv("output/results/secondary/2_case_control/Nitrate_res_case_control_p1_births_2020-08-23.csv") %>% 
  mutate(model = "Case control sample \n(first-parity births)")


res_lmp_boe <- read_csv("output/results/sensitivity/3_stratification/lmp_boe/") %>% 
  mutate(model = ifelse(method == "LMP", "2000-2006", "2007-2011")) %>% 
  filter(effect == "Main")

res_certainty <- read_csv("output/results/sensitivity/1_exposure_assumption/exp_certainty/Nitrate_res_certainty_case_control_2020-06-18.csv") %>% 
  filter(effect == "Main")

res_districts <- read_csv("output/results/sensitivity/3_stratification/region/Nitrate_res_district_all_2020-08-23.csv") %>% 
  rename(model = .id) %>% 
  mutate(model = case_when(
    model == "1. Northern" ~ "1. Northern California",
    model == "2. Bay Area" ~ "2. San Francisco Bay Area",
    model == "3. SJV" ~ "3. San Joaquin Valley",
    model == "4. LA" ~ "4. South Coast",
    TRUE ~ model))


res_sex <- read_csv("output/results/sensitivity/3_stratification/infant_sex/Nitrate_res_sex_case_control_2020-08-23.csv") %>% 
  mutate(model = ifelse(sex == 1, "Male", "Female")) 



table_strata_cohort <- bind_rows(res_primary, res_nomove,
                              res_race, res_education) %>%
  mutate(model = factor(model, levels = c("All siblings", 
                                          "Siblings born in the \nsame water system",
                                          "Race",
                                          "Education")),
         outcome = factor(outcome, labels = c("20-31 weeks", "32-36 weeks")),
         cat = paste(exposure_cat, outcome)) %>% 
  arrange(exposure_cat) %>% 
  select(model, model_cat, cat, result) %>% 
  spread(key = cat, value = result) 

