## FUNCTIONS FOR WITHIN-MOTHER SENSITIVITY ANALYSES
## Nitrate in drinking water and spontaneous preterm birth
## Author: A. Sherris

# functions to run conditional logistic regression models for within-mother analysis
# input: 
# data: data frame for analysis
# outcome: study outcome as a string identical to column name ("prem_20_to_31" or "prem_32_to_36)
# output: survival model object


# fully adjusted model

clog_adj_full <- function(data, outcome) {
  
  data$exp_cat <- factor(data$exp_cat, levels = c("low", "medium", "high"))
  
  clogit(data[[outcome]] ~ exp_cat +
           factor(parity_cat) +
           factor(ipi_less_1yr) +
           factor(mat_age_5cat) + 
           factor(payer) + 
           factor(precare) +
           strata(mat_id),
         method =  "exact",
         data = data)
}


clog_adj_yr <- function(data, outcome, exposure, carryover, method) {
  
  data$exp_cat <- factor(data$exp_cat, levels = c("low", "medium", "high"))
  
  clogit(data[[outcome]] ~ exp_cat +
           factor(parity_cat) +
           factor(ipi_less_1yr) +
           factor(mat_age_5cat) +
           factor(year) +
           strata(mat_id),
         method = "efron",
         data = data)
}


clog_adj_month_doc <- function(data, outcome, exposure, carryover, method) {
  
  data$exp_cat <- factor(data$exp_cat, levels = c("low", "medium", "high"))
  data$month <- month(data$doc)
  
  clogit(data[[outcome]] ~ exp_cat +
           factor(parity_cat) +
           factor(ipi_less_1yr) +
           factor(mat_age_5cat) +
           factor(year) +
           factor(month) + 
           strata(mat_id),
         method = "efron",
         data = data)
}


clog_adj_region <- function(data, outcome, exposure, carryover, method) {
  
  data$exp_cat <- factor(data$exp_cat, levels = c("low", "medium", "high"))
  data$month <- month(data$doc)
  
  data <- data %>% 
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
  
  clogit(data[[outcome]] ~ exp_cat +
           factor(parity_cat) +
           factor(ipi_less_1yr) +
           factor(mat_age_5cat) +
           factor(year) +
           factor(month) + 
           factor(district) +
           strata(mat_id),
         method = "efron",
         data = data)
}


clog_adj_agespl <- function(data, outcome, exposure, carryover, method) {
  
  data$exp_cat <- factor(data$exp_cat, levels = c("low", "medium", "high"))
  age_spl <- ns(data$mat_age, df = 5)
  
  clogit(data[[outcome]] ~ exp_cat +
           factor(parity_cat) +
           factor(ipi_less_1yr) +
           age_spl + 
           strata(mat_id),
         method = "efron",
         data = data)
}
