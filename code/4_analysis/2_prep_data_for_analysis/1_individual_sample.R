## PREP DATA FOR ANALYSIS: INDIVIDUAL-LEVEL SAMPLE
## Nitrate in drinking water and spontaneous preterm birth
## Author: A. Sherris

# load exposure assessment results

load("data/processed/births_exposure/births_exposure.RData")

# define sample for case-control (individual-level) analysis

births_case_control <- births_exposure %>%
  # restrict to those with complete exposure data
  filter(!is.na(exp_cat)) %>% 
  # define covariates
  mutate(
    parity_cat = case_when(
      parity == 1 ~ 1,
      parity == 2 ~ 2,
      parity > 2 ~ 3),
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
        county %in% c("SAN LUIS OBISPO", "SANTA BARBARA", "VENTURA", "LOS ANGELES") ~ "4. LA"),
    lmp_boe = ifelse(year < 2007, "LMP", "BOE"))

# save  
save(births_case_control, file =  "data_processed/births_exposure/births_case_control.RData")
