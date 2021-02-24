# lmp / BOE
  
  results_lmp_boe <- read_csv("output/results/sensitivity/3_stratification/lmp_boe/Nitrate_results_lmp_boe_2020-08-22.csv") %>% 
    mutate(model = ifelse(method == "LMP", "2000-2006", "2007-2011")) %>% 
    filter(effect == "Main")
  
  plot <- plot_model_compare(results_lmp_boe, "Time Period")
  plot
  ggsave("output/visualizations/2_supplement/Fig_S9_LMP.png",
          plot, device = "png", width = 7.5, dpi = 150, height = 4, units = "in")

  
# exposure certainty
  
  res_certainty <- read_csv("output/results/sensitivity/1_exposure_assumption/exp_certainty/Nitrate_res_certainty_case_control_2020-06-18.csv") %>% 
    filter(effect == "Main")
  
# district
  
    res_districts <- read_csv("output/results/sensitivity/3_stratification/region/Nitrate_res_district_all_2020-08-23.csv") %>% 
      rename(model = .id) %>% 
      mutate(model = case_when(
        model == "1. Northern" ~ "1. Northern California",
        model == "2. Bay Area" ~ "2. San Francisco Bay Area",
        model == "3. SJV" ~ "3. San Joaquin Valley",
        model == "4. LA" ~ "4. South Coast",
        TRUE ~ model))
    
    plot <- plot_model_compare(res_districts, "District", legend_position = "none")
    plot
    ggsave("output/visualizations/2_supplement/Fig_S11_regions.png",
           plot, device = "png", width = 7.5, dpi = 150, height = 4, units = "in")
    
    
    
    county_regions <- ca_counties %>% 
      mutate(  district = case_when(
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
        county %in% c("SAN LUIS OBISPO", "SANTA BARBARA", "VENTURA", "LOS ANGELES") ~ "4. LA"),
        model = case_when(
        district == "1. Northern" ~ "1. Northern California",
        district == "2. Bay Area" ~ "2. San Francisco Bay Area",
        district == "3. SJV" ~ "3. San Joaquin Valley",
        district == "4. LA" ~ "4. South Coast",
        TRUE ~ district)) 
    
    ggplot(county_regions, aes(fill = reorder(model, desc(model)))) + 
      geom_sf() +
      scale_fill_lancet() +   
      guides(fill = guide_legend(reverse=T)) +
      theme_void() +
      ditch_axes
      
    ggsave("output/4_visualizations/2_supplement/districts_map.png")
    
    
    
    
# infant sex
    
    res_sex <- read_csv("output/results/sensitivity/3_stratification/infant_sex/Nitrate_res_sex_case_control_2020-08-23.csv") %>% 
      mutate(model = ifelse(sex == 1, "Male", "Female")) 

    plot <- plot_model_compare(res_sex, "Infant sex")
    plot
    ggsave("output/visualizations/2_supplement/Fig_S10_sex.png",
           plot, device = "png", width = 7.5, dpi = 150, height = 4, units = "in")
    
    
    
# maternal race
    
    res_race <- read_csv("output/results/sensitivity/3_stratification/maternal_demo/Nitrate_res_race_all_sibs_consec_2020-08-30.csv") %>% 
      filter(upper < 15) %>% 
      rename(model = race) %>% 
      filter(model != "Other",
             effect == "Main")
    
    plot_model_compare(res_race, "Race/Ethnicity")
    
     ggsave("output/4_visualizations/2_supplement/compare_maternal_race.png")
    
# maternal education
    
    res_education <- read_csv("output/results/sensitivity/3_stratification/maternal_demo/Nitrate_res_education_sibs_consec_2020-08-30.csv") %>% 
      rename(model = education) %>% 
      mutate(model = factor(model, 
                          levels = c("<12 years", "12 years", ">12 years"),
                          labels = c("Less than high school", "High school", 
                                     "More than high school"))) %>% 
      filter(effect == "Main")
    
    plot_model_compare(res_education,  "Level of education")
    
    ggsave("output/4_visualizations/2_supplement/compare_maternal_education.png")
    
