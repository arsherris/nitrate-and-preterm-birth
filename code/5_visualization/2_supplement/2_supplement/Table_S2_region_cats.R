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

# Exposure distribution by region

exposure_region <- data_region %>% 
  group_by(district) %>% 
  count(exp_cat) %>% 
  mutate(sum_n = sum(n),
         perc = n*100/sum_n,
         result = paste(n, " (", round(perc, 2), ")", sep = "")) %>% 
  select(district, exp_cat, result) %>% 
  spread(exp_cat, result)


write_output(exposure_region, "output/results/sensitivity/3_stratification/region/")

