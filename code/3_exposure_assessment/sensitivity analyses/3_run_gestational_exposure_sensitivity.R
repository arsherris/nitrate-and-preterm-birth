## RUN FUNCTIONS TO ASSIGN GESTATIONAL EXPOSURE
## Drinking water and PTB sybling analysis
## Author: A. Sherris

load("data_processed/births_exposure/births_study_pop.RData")
sibs_crossover <- read_csv("data_out/2_exposure_assessment/sibs_crossover_2020-04-13.csv")

births_crossover <- births_study_pop %>% 
  filter(birth_id %in% sibs_crossover$birth_id)

rm(births_study_pop, sibs_crossover)

# load functions

source("code/4_exposure_assessment/2_assign_gestational_exposure.R")
source("code/4_exposure_assessment/sensitivity analyses/1_assign_gestational_exposure_simple_avg.R")
source("code/4_exposure_assessment/sensitivity analyses/2_assign_gestational_exposure_mcl_notimeweight.R")

# generate exposure intervals for each birth
# primary interval is from conception to birth;
# secondary long interval is from 15 months before conception to 12 months after birth

births_crossover_interval <- births_crossover %>% 
  mutate(before_doc = doc - 450, # 15 months before conception
         after_bdate = bdate + 360, # 12 months after birth
         preg_int = interval(doc, bdate), # interval of pregnancy
         preg_int_long = interval(before_doc, after_bdate)) # longer interval before and after pregnancy


# generate list of counties in birth data;
  # these will be used to run county-by-county exposure assessment functions
  county_list <- unique(births_crossover_interval$county)

# assign nitrate exposure assuming flow contribution based on number of sources
  
Sys.time()
exposure_simple <- lapply(county_list,
                      FUN = function(x)
                        assign_exp_by_county(exposure_assign_fxn = assign_gest_exposure_simple, 
                                             births_pws_data = births_crossover_interval, 
                                             chemical = chem_of_interest, 
                                             county_name = x)) %>% bind_rows() 
Sys.time()

# assign nitrate exposure assuming flow contribution higher among lower concentration sources

exposure_low <- lapply(county_list,
                          FUN = function(x)
                            assign_exp_by_county(exposure_assign_fxn = assign_gest_exposure_low, 
                                                 births_pws_data = births_crossover_interval, 
                                                 chemical = chem_of_interest, 
                                                 county_name = x)) %>% bind_rows() 
Sys.time()


# assign nitrate exposure assuming flow contribution lower for above MCL sources

exposure_mcl <- lapply(county_list,
                       FUN = function(x)
                         assign_exp_by_county(exposure_assign_fxn = assign_gest_exposure_mcl, 
                                              births_pws_data = births_crossover_interval, 
                                              chemical = chem_of_interest, 
                                              county_name = x)) %>% bind_rows() 
Sys.time()





save(exposure_simple, file =  "data_processed/births_exposure/sensitivity/nitrate_exposure_simple_2020-07-18.RData")
save(exposure_low, file =  "data_processed/births_exposure/sensitivity/nitrate_exposure_low_2020-07-18.RData")
save(exposure_mcl, file =  "data_processed/births_exposure/sensitivity/nitrate_exposure_mcl.RData")

# join to births covariates
sibs_crossover_simple <- exposure_simple %>% 
  select(-pwsid) %>% 
  separate(conc, into = c("conc", "sample_n", "source_n", "uncertainty_SD", "uncertainty_SE", "interval"), 
           sep = " ", convert = T) %>% 
  mutate(mcl = first(wq_data$mcl),
         exp_cat = factor(case_when(
           conc < mcl / 2 ~ "low",
           conc >= mcl ~ "high",
           conc >= mcl/2 ~ "medium", # medium only includes less than MCL
           TRUE ~ NA_character_), 
           levels = c("low", "medium", "high")),
         chemical = chem_of_interest) %>% 
  left_join(births_crossover, by = "birth_id") %>%
  unique() %>% 
  group_by(mat_id) %>% 
  # find carryover effects
  mutate(exp_carryover =  factor(case_when(
    as.numeric(parity) > 1 & lag(exp_cat) == "high" ~ "high",
    as.numeric(parity) > 1 & lag(exp_cat) == "medium" ~ "medium", 
    TRUE ~ "low"), levels = c("low", "medium", "high"))) %>% 
  mutate(mat_race_locf = as.character(na.locf(mat_race, na.rm=F)),
         # if mother race is listed as NA, apply race from later siblings
         mat_race_locf = na.locf(mat_race_locf, fromLast = T, na.rm=F),
         mat_race_cln = case_when(
           # if there is only one race listed for all siblings, leave as is
           # if no race listed among siblings, leave NA
           length(unique(mat_race_locf)) == 1 ~ mat_race_locf,
           # if there are an odd number of births, use the most common race
           n() %% 2 != 0 ~ mat_race_locf[which.max(tabulate(match(mat_race_locf, unique(mat_race_locf))))], #names(which.max(table(mat_race_locf))),  # mat_race_locf[which.max(tabulate(match(mat_race_locf, unique(mat_race_locf))))]
           # if there are multiple races given, use the last listed race
           length(mat_race_locf) > 1 ~ last(mat_race_locf),
           TRUE ~ "Problem"),
         ipi_days = doc - lag(bdate)) %>% 
  ungroup() %>% 
  mutate(ipi_less_1yr = case_when(
           ipi_days < 365 & ipi_days > 30 ~ "yes", 
           ipi_days >= 365 ~ "no",
           is.na(ipi_days) ~ "unknown"),
         ipi_less_1yr = replace_na(ifelse(ipi_days < 365 & ipi_days > 30, "yes", "no"), "unknown"))

Sys.time()

# low
sibs_crossover_low <- exposure_low %>% 
  select(-pwsid) %>% 
  separate(conc, into = c("conc", "sample_n", "source_n", "uncertainty_SD", "uncertainty_SE", "interval"), 
           sep = " ", convert = T) %>% 
  mutate(mcl = first(wq_data$mcl),
         exp_cat = factor(case_when(
           conc < mcl / 2 ~ "low",
           conc >= mcl ~ "high",
           conc >= mcl/2 ~ "medium", # medium only includes less than MCL
           TRUE ~ NA_character_), 
           levels = c("low", "medium", "high")),
         chemical = chem_of_interest) %>% 
  left_join(births_crossover, by = "birth_id") %>%
  unique() %>% 
  group_by(mat_id) %>% 
  # find carryover effects
  mutate(exp_carryover =  factor(case_when(
    as.numeric(parity) > 1 & lag(exp_cat) == "high" ~ "high",
    as.numeric(parity) > 1 & lag(exp_cat) == "medium" ~ "medium", 
    TRUE ~ "low"), levels = c("low", "medium", "high"))) %>% 
  mutate(mat_race_locf = as.character(na.locf(mat_race, na.rm=F)),
         # if mother race is listed as NA, apply race from later siblings
         mat_race_locf = na.locf(mat_race_locf, fromLast = T, na.rm=F),
         mat_race_cln = case_when(
           # if there is only one race listed for all siblings, leave as is
           # if no race listed among siblings, leave NA
           length(unique(mat_race_locf)) == 1 ~ mat_race_locf,
           # if there are an odd number of births, use the most common race
           n() %% 2 != 0 ~ mat_race_locf[which.max(tabulate(match(mat_race_locf, unique(mat_race_locf))))], #names(which.max(table(mat_race_locf))),  # mat_race_locf[which.max(tabulate(match(mat_race_locf, unique(mat_race_locf))))]
           # if there are multiple races given, use the last listed race
           length(mat_race_locf) > 1 ~ last(mat_race_locf),
           TRUE ~ "Problem"),
         ipi_days = doc - lag(bdate)) %>% 
  ungroup() %>% 
  mutate(ipi_less_1yr = case_when(
    ipi_days < 365 & ipi_days > 30 ~ "yes", 
    ipi_days >= 365 ~ "no",
    is.na(ipi_days) ~ "unknown"),
    ipi_less_1yr = replace_na(ifelse(ipi_days < 365 & ipi_days > 30, "yes", "no"), "unknown"))

Sys.time()


# mcl
sibs_crossover_mcl <- exposure_mcl %>% 
  select(-pwsid) %>% 
  separate(conc, into = c("conc", "sample_n", "source_n", "uncertainty_SD", "uncertainty_SE", "interval"), 
           sep = " ", convert = T) %>% 
  mutate(mcl = first(wq_data$mcl),
         exp_cat = factor(case_when(
           conc < mcl / 2 ~ "low",
           conc >= mcl ~ "high",
           conc >= mcl/2 ~ "medium", # medium only includes less than MCL
           TRUE ~ NA_character_), 
           levels = c("low", "medium", "high")),
         chemical = chem_of_interest) %>% 
  left_join(births_crossover, by = "birth_id") %>%
  unique() %>% 
  group_by(mat_id) %>% 
  # find carryover effects
  mutate(exp_carryover =  factor(case_when(
    as.numeric(parity) > 1 & lag(exp_cat) == "high" ~ "high",
    as.numeric(parity) > 1 & lag(exp_cat) == "medium" ~ "medium", 
    TRUE ~ "low"), levels = c("low", "medium", "high"))) %>% 
  mutate(mat_race_locf = as.character(na.locf(mat_race, na.rm=F)),
         # if mother race is listed as NA, apply race from later siblings
         mat_race_locf = na.locf(mat_race_locf, fromLast = T, na.rm=F),
         mat_race_cln = case_when(
           # if there is only one race listed for all siblings, leave as is
           # if no race listed among siblings, leave NA
           length(unique(mat_race_locf)) == 1 ~ mat_race_locf,
           # if there are an odd number of births, use the most common race
           n() %% 2 != 0 ~ mat_race_locf[which.max(tabulate(match(mat_race_locf, unique(mat_race_locf))))], #names(which.max(table(mat_race_locf))),  # mat_race_locf[which.max(tabulate(match(mat_race_locf, unique(mat_race_locf))))]
           # if there are multiple races given, use the last listed race
           length(mat_race_locf) > 1 ~ last(mat_race_locf),
           TRUE ~ "Problem"),
         ipi_days = doc - lag(bdate)) %>% 
  ungroup() %>% 
  mutate(ipi_less_1yr = case_when(
    ipi_days < 365 & ipi_days > 30 ~ "yes", 
    ipi_days >= 365 ~ "no",
    is.na(ipi_days) ~ "unknown"),
    ipi_less_1yr = replace_na(ifelse(ipi_days < 365 & ipi_days > 30, "yes", "no"), "unknown"))

Sys.time()

# save exposure dataset
save(sibs_crossover_simple,  file = "data_processed/births_exposure/sensitivity/sibs_crossover_simple_2020-07-19.RData")
save(sibs_crossover_low,  file = "data_processed/births_exposure/sensitivity/sibs_crossover_low_2020-07-19.RData")
save(sibs_crossover_mcl, file = "data_processed/births_exposure/sensitivity/sibs_crossover_mcl.RData")
