## FUNCTIONS TO GENERATE TABLES
## Nitrate in drinking water and spontaneous preterm birth
## Author: A. Sherris


# function to prepare data for table visualization
  # cleans, renames, and reorders variables
  # input: data to be visualized
  # output: cleaned data to be visualized 

prep_tbl_data <- function(data) {
  
  # clean variable levels
  
  tbl_data <- data %>% 
    mutate(prem_3cat =  factor(case_when(prem_5cat %in% 1:3 ~ 1,
                                         prem_5cat == 4 ~ 2,
                                         prem_5cat == 5 ~ 3),
                               labels = c("1 20-31 weeks", "2 32-36 weeks", "3 >= 37 weeks")),
           mat_race = factor(mat_race, 
                                 levels = c("Hispanic", "White", "Asian", "Black", "Other"),
                                 labels = c("1 Hispanic", "2 White", "3 Asian", "4 Black", "5 Other")),
           mat_educ = factor(mat_educ, 
                             levels = c("<12 years", "12 years", ">12 years"),
                             labels = c("1 Less than high school", "2 High school", "3 More than high school")),
           mat_age_5cat = factor(mat_age_5cat,
                                 levels = c("<20", "20-24", "25-29", "30-34", ">=35"),
                                 labels = c("20", "20-24", "25-29", "30-34", "35")),
           payer = factor(payer, 
                          levels = c("Medi-Cal", "Private", "Uninsured", "Other"),
                          labels = c("1 Medi-Cal", "2 Private", "3 Other", "4 Uninsured")),
           precare = factor(precare, levels  = c("Yes", "No"),
                            labels = c("1 Yes", "2 No")),
           sex = factor(sex, levels = c(2, 1), labels = c("Male", "Female")),
           parity_cat = case_when(parity == 1 ~ 1,
                                  parity == 2 ~ 2,
                                  TRUE ~ 3),
           region = case_when(
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
           month = lubridate::month(doc),
           count = "count")
    
  
  tbl_data$n_mothers = length(unique(tbl_data$mat_id))
  
  # specify output
  return(tbl_data)
  
}

# function to generate "Table 1"
  # counts the number and percent of births in each covariate level
  # input: data to be summarized
  # output: Table 1

make_table1 <- function(data) {
  data %>% 
    select(count, mat_race, mat_age_5cat, mat_educ, payer, precare, sex, parity_cat, 
           year, month, region) %>% 
    gather(variable, value) %>% 
    count(variable, value) %>% 
    group_by(variable) %>%
    mutate(percent = signif(n*100 / sum(n), digits = 2)) %>% 
    ungroup() %>% 
    mutate(variable = factor(variable, 
                             levels = c("count", "mat_age_5cat", "mat_race", "mat_educ", 
                                        "payer", "precare",  "parity_cat", "sex", 
                                        "year", "month", "region"),
                             labels = c("Count", "Age", "Race/ethnicity", "Education",
                                        "Payer", "Prenatal care", "Parity", "Infant sex",
                                        "Year", "Month", "Region"))) %>% 
    arrange(variable, value) %>% 
    return()
  
}


# function to generate table 2
  # counts the number of births in each exposure and outcome category
  # input: data to be summarized
  # output: Table 2

make_table2 <- function(data) {
  
  table2 <- data %>% 
    mutate(prem_3cat = case_when(prem_5cat_spon_nocomp == 5 ~ "3 Term",
                                 prem_32_to_36 == 1 ~ "2 Near-term",
                                 prem_20_to_31 == 1 ~ "1 Early")) %>% 
    group_by(prem_3cat) %>% 
    count(exp_cat, name = "num") %>% 
    ungroup %>% 
    spread(exp_cat, num) %>% 
    mutate(total = low + medium + high) %>% 
    return()
}


## END