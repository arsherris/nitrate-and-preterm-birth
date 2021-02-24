## sensitivity analysis: by region

# load data

# split data by region
data_region <- births_case_control %>% 
  mutate(
    district = case_when(
      county %in% c("BUTTE", "COLUSA",  "GLENN","SHASTA",  "TEHAMA", 
                    "EL DORADO","PLACER","YOLO", "YUBA","SUTTER", "SACRAMENTO",
                    "DEL NORTE", "HUMBOLDT" ,"SISKIYOU",   "SIERRA",  "NEVADA",
                    "LASSEN", "MODOC", "PLUMAS","TRINITY",
                    "AMADOR", "CALAVERAS") ~ "1. Northern",
      county %in% c("MENDOCINO","LAKE", "ALAMEDA", "CONTRA COSTA", "MARIN",  "SONOMA",
                    "SAN FRANCISCO", "SAN MATEO", "SANTA CLARA", "SOLANO",
                    "MONTEREY",  "NAPA","SAN BENITO", "SANTA CRUZ") ~ "2. Bay Area",
      county %in% c("FRESNO", "KERN", 'KINGS',"MERCED","SAN JOAQUIN", "MADERA",  
                    "STANISLAUS", "TULARE", "MARIPOSA", "TUOLUMNE") ~ "3. SJV",
      county %in% c("MONO", "INYO", "ALPINE", "RIVERSIDE", "SAN BERNARDINO", 
                    "ORANGE", "IMPERIAL", "SAN DIEGO") ~ "5. Inland Empire",
      county %in% c("SAN LUIS OBISPO", "SANTA BARBARA", "VENTURA", "LOS ANGELES") ~ "4. LA")) %>% 
  mutate(district = factor(district))

table(data_region$district)

# interaction term

data_region$district <- factor(data_region$district, levels = c("5. Inland Empire",
                                                                "1. Northern",
                                                                "2. Bay Area",
                                                                "3. SJV",
                                                                "4. LA"))

res_interaction_region <- res_table_interactions(data_region, glmer_interaction, "district")

write_output(res_interaction_region, "output/results/sensitivity/3_stratification/region/")


## split by district

  # split
  data_district <- split(data_region, data_region$district)
    
  # results by region
  res_district <- map_df(data_district, 
                         function(x) res_table_primary(x, glmer_case_control_adj),
                         .id = "District") 
  
  
  # save
  write_output(res_district, "output/results/sensitivity/3_stratification/region/")

## compare model with and without interaction
  
  mod_district_early <-  glmer(prem_20_to_31 ~ exp_cat +
                                  district +
                                  factor(parity_cat) +
                                  payer + precare +
                                  mat_race + mat_educ + mat_age_5cat +
                                  (1|pwsid) + (1|mat_id),
                                family = binomial, 
                                control = fast_control,
                                nAGQ = 0,
                                data = data_region)
  
  mod_district_late <-  glmer(prem_32_to_36 ~ exp_cat +
                                 district +
                                 factor(parity_cat) +
                                 payer + precare +
                                 mat_race + mat_educ + mat_age_5cat +
                                 (1|pwsid) + (1|mat_id),
                               family = binomial, 
                               control = fast_control,
                               nAGQ = 0,
                               data = data_region)
  
  
  mod_district_early_int <- update(mod_district_early, . ~ . + exp_cat*district)
  mod_district_late_int  <- update(mod_district_late,  . ~ . + exp_cat*district)
  
  save(mod_district_early, mod_district_late, mod_district_early_int, mod_district_late_int,
       file = "output/results/sensitivity/3_stratification/region/district_mods.RData")
  
  
  lrtest(mod_district_early_int, mod_district_early)
  lrtest(mod_district_late_int, mod_district_late)
  
